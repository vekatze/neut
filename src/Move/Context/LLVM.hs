module Move.Context.LLVM
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
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO, raiseError')
import Move.Context.Env qualified as Env
import Move.Context.External qualified as External
import Move.Context.Path qualified as Path
import Move.Context.Platform qualified as Platform
import Move.Context.ProcessRunner qualified as ProcessRunner
import Move.Scene.Init.Base qualified as Base
import Path
import Path.IO
import Rule.Config.Build
import Rule.Module (extractModule)
import Rule.OutputKind qualified as OK
import Rule.Source
import Rule.Target
import System.Process (CmdSpec (RawCommand))

data Handle
  = Handle
  { debugHandle :: Debug.Handle,
    pathHandle :: Path.Handle,
    externalHandle :: External.Handle,
    envHandle :: Env.Handle
  }

new :: Base.Handle -> Handle
new (Base.Handle {..}) = do
  let externalHandle = External.new debugHandle
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
      liftIO $ Debug.report (debugHandle h) $ "Saving: " <> T.pack (toFilePath path)
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
  liftIO $ Debug.report (debugHandle h) $ "Executing: " <> T.pack (show (clang, optionList))
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
