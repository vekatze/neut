module Kernel.Move.Context.LLVM
  ( Handle,
    new,
    emit,
    link,
  )
where

import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Time.Clock
import Error.Rule.EIO (EIO)
import Kernel.Common.Rule.Module (extractModule)
import Kernel.Common.Rule.OutputKind qualified as OK
import Kernel.Common.Rule.Source
import Kernel.Common.Rule.Target
import Kernel.Move.Context.Global.Env qualified as Env
import Kernel.Move.Context.Global.Path qualified as Path
import Kernel.Move.Context.Global.Platform qualified as Platform
import Kernel.Move.Scene.RunProcess qualified as RunProcess
import Kernel.Move.Scene.Init.Global qualified as Global
import Language.Common.Rule.Error (newError')
import Logger.Move.Debug qualified as Logger
import Logger.Rule.Handle qualified as Logger
import Path
import Path.IO
import Path.Move.Write (writeLazyByteString)
import System.Process (CmdSpec (RawCommand))

data Handle = Handle
  { loggerHandle :: Logger.Handle,
    pathHandle :: Path.Handle,
    runProcessHandle :: RunProcess.Handle,
    envHandle :: Env.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  let runProcessHandle = RunProcess.new loggerHandle
  Handle {..}

type ClangOption = String

type LLVMCode = L.ByteString

emit ::
  Handle ->
  Target ->
  [ClangOption] ->
  UTCTime ->
  Either MainTarget Source ->
  [OK.OutputKind] ->
  L.ByteString ->
  EIO ()
emit h target clangOptions timeStamp sourceOrNone outputKindList llvmCode = do
  case sourceOrNone of
    Right source -> do
      kindPathList <- zipWithM (Path.attachOutputPath (pathHandle h) target) outputKindList (repeat source)
      forM_ kindPathList $ \(_, outputPath) -> ensureDir $ parent outputPath
      emitAll h clangOptions llvmCode kindPathList
      forM_ (map snd kindPathList) $ \path -> do
        setModificationTime path timeStamp
    Left t -> do
      let mainModule = Env.getMainModule (envHandle h)
      let mm = extractModule mainModule
      kindPathList <- zipWithM (Path.getOutputPathForEntryPoint (pathHandle h) mm) outputKindList (repeat t)
      forM_ kindPathList $ \(_, path) -> ensureDir $ parent path
      emitAll h clangOptions llvmCode kindPathList
      forM_ (map snd kindPathList) $ \path -> do
        setModificationTime path timeStamp

emitAll :: Handle -> [ClangOption] -> LLVMCode -> [(OK.OutputKind, Path Abs File)] -> EIO ()
emitAll h clangOptions llvmCode kindPathList = do
  case kindPathList of
    [] ->
      return ()
    (kind, path) : rest -> do
      emit' h clangOptions llvmCode kind path
      emitAll h clangOptions llvmCode rest

emit' :: Handle -> [ClangOption] -> LLVMCode -> OK.OutputKind -> Path Abs File -> EIO ()
emit' h clangOptString llvmCode kind path = do
  case kind of
    OK.LLVM -> do
      liftIO $ Logger.report (loggerHandle h) $ "Saving: " <> T.pack (toFilePath path)
      liftIO $ writeLazyByteString path llvmCode
    OK.Object ->
      emitInner h clangOptString llvmCode path

emitInner :: Handle -> [ClangOption] -> L.ByteString -> Path Abs File -> EIO ()
emitInner h additionalClangOptions llvm outputPath = do
  clang <- liftIO Platform.getClang
  let optionList = clangBaseOpt outputPath ++ additionalClangOptions
  let spec =
        RunProcess.Spec
          { cmdspec = RawCommand clang optionList,
            cwd = Nothing
          }
  value <- liftIO (RunProcess.run10 (runProcessHandle h) spec (RunProcess.Lazy llvm))
  case value of
    Right _ ->
      return ()
    Left err ->
      throwError $ newError' err

clangBaseOpt :: Path Abs File -> [String]
clangBaseOpt outputPath =
  [ "-xir",
    "-Wno-override-module",
    "-O2",
    "-flto=thin",
    "-c",
    "-",
    "-o",
    toFilePath outputPath
  ]

link :: Handle -> [String] -> [Path Abs File] -> Path Abs File -> EIO ()
link h clangOptions objectPathList outputPath = do
  clang <- liftIO Platform.getClang
  ensureDir $ parent outputPath
  RunProcess.run (runProcessHandle h) clang $ clangLinkOpt objectPathList outputPath (unwords clangOptions)

clangLinkOpt :: [Path Abs File] -> Path Abs File -> String -> [String]
clangLinkOpt objectPathList outputPath additionalOptionStr = do
  let pathList = map toFilePath objectPathList
  [ "-Wno-override-module",
    "-O2",
    "-flto=thin",
    "-pthread",
    "-lm",
    "-o",
    toFilePath outputPath
    ]
    ++ words additionalOptionStr
    ++ pathList
