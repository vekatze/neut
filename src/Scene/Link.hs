module Scene.Link
  ( link,
  )
where

import Context.App
import Context.Debug (report)
import Context.Env qualified as Env
import Context.LLVM qualified as LLVM
import Context.Path qualified as Path
import Context.Remark qualified as Remark
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Containers.ListUtils (nubOrdOn)
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Entity.Artifact qualified as A
import Entity.Module
import Entity.OutputKind qualified as OK
import Entity.Source qualified as Source
import Entity.Target
import Path
import Path.IO
import System.Console.ANSI
import System.IO

link :: MainTarget -> Bool -> Bool -> A.ArtifactTime -> [Source.Source] -> App ()
link target shouldSkipLink didPerformForeignCompilation artifactTime sourceList = do
  mainModule <- Env.getMainModule
  isExecutableAvailable <- Path.getExecutableOutputPath target mainModule >>= Path.doesFileExist
  let freshExecutableAvailable = isJust (A.objectTime artifactTime) && isExecutableAvailable
  if shouldSkipLink || (not didPerformForeignCompilation && freshExecutableAvailable)
    then report "Skipped linking object files"
    else link' target mainModule sourceList

link' :: MainTarget -> Module -> [Source.Source] -> App ()
link' target mainModule sourceList = do
  liftIO $ putStr "⠋ Linking.."
  liftIO $ hFlush stdout
  mainObject <- snd <$> Path.getOutputPathForEntryPoint mainModule OK.Object target
  outputPath <- Path.getExecutableOutputPath target mainModule
  objectPathList <- mapM (Path.sourceToOutputPath (Main target) OK.Object) sourceList
  let moduleList = nubOrdOn moduleID $ map Source.sourceModule sourceList
  foreignDirList <- mapM (Path.getForeignDir (Main target)) moduleList
  foreignObjectList <- concat <$> mapM getForeignDirContent foreignDirList
  let clangOptions = getLinkOption (Main target)
  let objects = mainObject : objectPathList ++ foreignObjectList
  LLVM.link clangOptions objects outputPath
  let numOfObjects = length objects
  let suffix = if numOfObjects <= 1 then "" else "s"
  finalizeProgressBar $ "Linked " <> T.pack (show numOfObjects) <> " object" <> suffix

getForeignDirContent :: Path Abs Dir -> App [Path Abs File]
getForeignDirContent foreignDir = do
  b <- doesDirExist foreignDir
  if b
    then snd <$> listDirRecur foreignDir
    else return []

finalizeProgressBar :: T.Text -> App ()
finalizeProgressBar title = do
  check <- Remark.withSGR [SetColor Foreground Vivid Green] "✓"
  let title' = check <> " " <> title
  liftIO $ B.hPutStr stdout $ encodeUtf8 "\r"
  liftIO clearFromCursorToLineEnd
  liftIO $ B.hPutStr stdout $ encodeUtf8 $ "\r" <> title' <> "\n"
