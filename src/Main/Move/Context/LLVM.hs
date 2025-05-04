module Main.Move.Context.LLVM
  ( Handle,
    new,
    emit,
    link,
    ensureSetupSanity,
  )
where

import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Time.Clock
import Logger.Move.Debug qualified as Logger
import Logger.Rule.Handle qualified as Logger
import Main.Move.Context.EIO (EIO, raiseError')
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.External qualified as External
import Main.Move.Context.Path qualified as Path
import Main.Move.Context.Platform qualified as Platform
import Main.Move.Context.ProcessRunner qualified as ProcessRunner
import Main.Move.Scene.Init.Base qualified as Base
import Main.Rule.Config.Build
import Main.Rule.Module (extractModule)
import Main.Rule.OutputKind qualified as OK
import Main.Rule.Source
import Main.Rule.Target
import Path
import Path.IO
import System.Process (CmdSpec (RawCommand))

data Handle = Handle
  { loggerHandle :: Logger.Handle,
    pathHandle :: Path.Handle,
    externalHandle :: External.Handle,
    envHandle :: Env.Handle
  }

new :: Base.Handle -> Handle
new (Base.Handle {..}) = do
  let externalHandle = External.new loggerHandle
  Handle {..}

type ClangOption = String

type LLVMCode = L.ByteString

ensureSetupSanity :: Config -> EIO ()
ensureSetupSanity cfg = do
  let willBuildObjects = OK.Object `elem` outputKindList cfg
  let willLink = not $ shouldSkipLink cfg
  when (not willBuildObjects && willLink) $
    raiseError' "`--skip-link` must be set explicitly when `--emit` does not contain `object`"

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
      liftIO $ Path.writeByteString path llvmCode
    OK.Object ->
      emitInner h clangOptString llvmCode path

emitInner :: Handle -> [ClangOption] -> L.ByteString -> Path Abs File -> EIO ()
emitInner h additionalClangOptions llvm outputPath = do
  clang <- liftIO Platform.getClang
  let optionList = clangBaseOpt outputPath ++ additionalClangOptions
  let spec =
        ProcessRunner.Spec
          { cmdspec = RawCommand clang optionList,
            cwd = Nothing
          }
  liftIO $ Logger.report (loggerHandle h) $ "Executing: " <> T.pack (show (clang, optionList))
  value <- liftIO $ ProcessRunner.run10 spec (ProcessRunner.Lazy llvm)
  case value of
    Right _ ->
      return ()
    Left err ->
      throwError $ ProcessRunner.toCompilerError err

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
  External.run (externalHandle h) clang $ clangLinkOpt objectPathList outputPath (unwords clangOptions)

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
