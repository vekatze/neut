module Command.Common.Build.Generate
  ( Handle,
    new,
    generateObject,
    generateAsm,
  )
where

import App.App (App)
import App.Error (newError')
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as L
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
    runProcessHandle :: RunProcess.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  let runProcessHandle = RunProcess.new loggerHandle
  Handle {..}

type ClangOption = String

type LLVMCode = L.ByteString

generateObject ::
  Handle ->
  Target ->
  [ClangOption] ->
  UTCTime ->
  Either MainTarget Source ->
  L.ByteString ->
  App ()
generateObject h target clangOptions timeStamp sourceOrNone llvmCode = do
  case sourceOrNone of
    Right source -> do
      (_, outputPath) <- Path.attachOutputPath (pathHandle h) target OK.Object source
      ensureDir $ parent outputPath
      generateObject' h clangOptions llvmCode outputPath
      setModificationTime outputPath timeStamp
    Left mainTarget -> do
      (_, outputPath) <- Path.getOutputPathForEntryPoint (pathHandle h) OK.Object mainTarget
      ensureDir $ parent outputPath
      generateObject' h clangOptions llvmCode outputPath
      setModificationTime outputPath timeStamp

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

generateObject' :: Handle -> [ClangOption] -> L.ByteString -> Path Abs File -> App ()
generateObject' h additionalClangOptions llvm outputPath = do
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
