module Command.Common.Build.Generate
  ( Handle,
    new,
    generateObject,
    flushObjects,
    generateAsm,
  )
where

import App.App (App)
import App.Error (newError')
import App.Run (forP_)
import Control.Monad (forM_)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as L
import Data.Char (isAlphaNum, isAscii)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.OutputKind qualified as OK
import Kernel.Common.RunProcess qualified as RunProcess
import Kernel.Common.Source
import Kernel.Common.Target
import Logger.Debug qualified as Logger
import Logger.Handle qualified as Logger
import Path
import Path.IO
import Path.Write (writeLazyByteString)
import System.Process (CmdSpec (RawCommand))

data Handle = Handle
  { loggerHandle :: Logger.Handle,
    pathHandle :: Path.Handle,
    platformHandle :: Platform.Handle,
    runProcessHandle :: RunProcess.Handle,
    stagingDir :: Path Abs Dir,
    numCapabilities :: Int,
    nextObjectIndexRef :: IORef Int,
    pendingObjectListRef :: IORef [PendingObject]
  }

data PendingObject = PendingObject
  { stagingLLVMPath :: Path Rel File,
    stagingObjectPath :: Path Rel File,
    finalObjectPath :: Path Abs File,
    objectTimeStamp :: UTCTime,
    additionalClangOptions :: [ClangOption]
  }

new :: Global.Handle -> Path Abs Dir -> Int -> IO Handle
new (Global.Handle {..}) stagingDir numCapabilities = do
  let runProcessHandle = RunProcess.new loggerHandle
  nextObjectIndexRef <- newIORef 0
  pendingObjectListRef <- newIORef []
  return $ Handle {..}

type ClangOption = String

type LLVMCode = L.ByteString

generateObject ::
  Handle ->
  Target ->
  [ClangOption] ->
  UTCTime ->
  T.Text ->
  Either MainTarget Source ->
  L.ByteString ->
  App ()
generateObject h target clangOptions timeStamp sourceLabel sourceOrNone llvmCode = do
  case sourceOrNone of
    Right source -> do
      (_, outputPath) <- Path.attachOutputPath (pathHandle h) target OK.Object source
      ensureDir $ parent outputPath
      stageObject h sourceLabel clangOptions llvmCode outputPath timeStamp
    Left mainTarget -> do
      (_, outputPath) <- Path.getOutputPathForEntryPoint (pathHandle h) OK.Object mainTarget
      ensureDir $ parent outputPath
      stageObject h sourceLabel clangOptions llvmCode outputPath timeStamp

generateAsm ::
  Handle ->
  Target ->
  UTCTime ->
  Either MainTarget Source ->
  L.ByteString ->
  App ()
generateAsm h target timeStamp sourceOrNone llvmCode = do
  case sourceOrNone of
    Right source -> do
      (_, outputPath) <- Path.attachOutputPath (pathHandle h) target OK.LLVM source
      ensureDir $ parent outputPath
      generateAsm' h llvmCode outputPath
      setModificationTime outputPath timeStamp
    Left mainTarget -> do
      (_, outputPath) <- Path.getOutputPathForEntryPoint (pathHandle h) OK.LLVM mainTarget
      ensureDir $ parent outputPath
      generateAsm' h llvmCode outputPath
      setModificationTime outputPath timeStamp

generateAsm' :: Handle -> LLVMCode -> Path Abs File -> App ()
generateAsm' h llvmCode path = do
  liftIO $ Logger.report (loggerHandle h) $ "Saving: " <> T.pack (toFilePath path)
  liftIO $ writeLazyByteString path llvmCode

stageObject :: Handle -> T.Text -> [ClangOption] -> L.ByteString -> Path Abs File -> UTCTime -> App ()
stageObject h sourceLabel additionalClangOptions llvm outputPath timeStamp = do
  objectIndex <- liftIO $ atomicModifyIORef' (nextObjectIndexRef h) $ \index -> (index + 1, index)
  let stagingBaseName = makeStagingBaseName objectIndex sourceLabel
  llvmPath <- parseRelFile $ stagingBaseName <> ".ll"
  objectPath <- parseRelFile $ stagingBaseName <> ".o"
  liftIO $ writeLazyByteString (stagingDir h </> llvmPath) llvm
  let pendingObject =
        PendingObject
          { stagingLLVMPath = llvmPath,
            stagingObjectPath = objectPath,
            finalObjectPath = outputPath,
            objectTimeStamp = timeStamp,
            additionalClangOptions
          }
  liftIO $ atomicModifyIORef' (pendingObjectListRef h) $ \pendingList -> (pendingObject : pendingList, ())

makeStagingBaseName :: Int -> T.Text -> String
makeStagingBaseName objectIndex sourceLabel = do
  let normalizedLabel = T.take 120 $ T.map normalizeStagingFileNameChar sourceLabel
  let label = if T.null normalizedLabel then "object" else T.unpack normalizedLabel
  show objectIndex <> "-" <> label

normalizeStagingFileNameChar :: Char -> Char
normalizeStagingFileNameChar char
  | isAscii char && (isAlphaNum char || char `elem` ("-_." :: String)) =
      char
  | otherwise =
      '#'

flushObjects :: Handle -> App ()
flushObjects h = do
  pendingObjectList <- liftIO $ readIORef (pendingObjectListRef h)
  let objectMap = Map.fromListWith (<>) $ map (\object -> (additionalClangOptions object, [object])) pendingObjectList
  forM_ (Map.toList objectMap) $ \(clangOptions, objectList) -> do
    let batchSize = max 1 $ ceilingDiv (length objectList) (numCapabilities h)
    forP_ (chunksOf batchSize objectList) $ compileObjectBatch h clangOptions

compileObjectBatch :: Handle -> [ClangOption] -> [PendingObject] -> App ()
compileObjectBatch h clangOptions objectList = do
  clang <- liftIO Platform.getClang
  let targetTriple = Platform.getClangTargetTriple (platformHandle h)
  let inputPathList = map (toFilePath . stagingLLVMPath) objectList
  let optionList = clangBaseOpt targetTriple ++ clangOptions ++ inputPathList
  let spec =
        RunProcess.Spec
          { cmdspec = RawCommand clang optionList,
            cwd = Just $ toFilePath $ stagingDir h
          }
  value <- liftIO $ RunProcess.run00 (runProcessHandle h) spec
  case value of
    Right _ -> do
      forM_ objectList $ \object -> do
        copyFile (stagingDir h </> stagingObjectPath object) (finalObjectPath object)
        setModificationTime (finalObjectPath object) (objectTimeStamp object)
    Left err ->
      throwError $ newError' err

clangBaseOpt :: String -> [String]
clangBaseOpt targetTriple =
  [ "-xir",
    "-target",
    targetTriple,
    "-O2",
    "-flto=thin",
    "-c"
  ]

ceilingDiv :: Int -> Int -> Int
ceilingDiv numerator denominator =
  (numerator + denominator - 1) `div` denominator

chunksOf :: Int -> [a] -> [[a]]
chunksOf size xs = do
  case xs of
    [] ->
      []
    _ -> do
      let (chunk, rest) = splitAt size xs
      chunk : chunksOf size rest
