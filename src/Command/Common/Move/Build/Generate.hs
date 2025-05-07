module Command.Common.Move.Build.Generate
  ( Handle,
    new,
    generateObject,
    generateAsm,
  )
where

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
import Kernel.Move.Scene.Init.Global qualified as Global
import Kernel.Move.Scene.RunProcess qualified as RunProcess
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

generateObject ::
  Handle ->
  Target ->
  [ClangOption] ->
  UTCTime ->
  Either MainTarget Source ->
  L.ByteString ->
  EIO ()
generateObject h target clangOptions timeStamp sourceOrNone llvmCode = do
  case sourceOrNone of
    Right source -> do
      (_, outputPath) <- Path.attachOutputPath (pathHandle h) target OK.Object source
      ensureDir $ parent outputPath
      generateObject' h clangOptions llvmCode outputPath
      setModificationTime outputPath timeStamp
    Left mainTarget -> do
      let mainModule = Env.getMainModule (envHandle h)
      let mm = extractModule mainModule
      (_, outputPath) <- Path.getOutputPathForEntryPoint (pathHandle h) mm OK.Object mainTarget
      ensureDir $ parent outputPath
      generateObject' h clangOptions llvmCode outputPath
      setModificationTime outputPath timeStamp

generateAsm ::
  Handle ->
  Target ->
  UTCTime ->
  Either MainTarget Source ->
  L.ByteString ->
  EIO ()
generateAsm h target timeStamp sourceOrNone llvmCode = do
  case sourceOrNone of
    Right source -> do
      (_, outputPath) <- Path.attachOutputPath (pathHandle h) target OK.LLVM source
      ensureDir $ parent outputPath
      generateAsm' h llvmCode outputPath
      setModificationTime outputPath timeStamp
    Left mainTarget -> do
      let mainModule = Env.getMainModule (envHandle h)
      let mm = extractModule mainModule
      (_, outputPath) <- Path.getOutputPathForEntryPoint (pathHandle h) mm OK.LLVM mainTarget
      ensureDir $ parent outputPath
      generateAsm' h llvmCode outputPath
      setModificationTime outputPath timeStamp

generateAsm' :: Handle -> LLVMCode -> Path Abs File -> EIO ()
generateAsm' h llvmCode path = do
  liftIO $ Logger.report (loggerHandle h) $ "Saving: " <> T.pack (toFilePath path)
  liftIO $ writeLazyByteString path llvmCode

generateObject' :: Handle -> [ClangOption] -> L.ByteString -> Path Abs File -> EIO ()
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
