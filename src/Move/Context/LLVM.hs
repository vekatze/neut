module Move.Context.LLVM
  ( emit,
    link,
    ensureSetupSanity,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Time.Clock
import Move.Context.App
import Move.Context.Debug (report)
import Move.Context.Env (getMainModule)
import Move.Context.External qualified as External
import Move.Context.Path qualified as Path
import Move.Context.Throw qualified as Throw
import Path
import Path.IO
import Rule.Config.Build
import Rule.Module (MainModule (MainModule))
import Rule.OutputKind qualified as OK
import Rule.ProcessRunner.Context.IO qualified as ProcessRunner (ioRunner)
import Rule.ProcessRunner.Rule qualified as ProcessRunner
import Rule.Source
import Rule.Target
import System.Process (CmdSpec (RawCommand))

type ClangOption = String

type LLVMCode = L.ByteString

ensureSetupSanity :: Config -> App ()
ensureSetupSanity cfg = do
  let willBuildObjects = OK.Object `elem` outputKindList cfg
  let willLink = not $ shouldSkipLink cfg
  when (not willBuildObjects && willLink) $
    Throw.raiseError' "`--skip-link` must be set explicitly when `--emit` does not contain `object`"

emit :: Target -> [ClangOption] -> UTCTime -> Either MainTarget Source -> [OK.OutputKind] -> L.ByteString -> App ()
emit target clangOptions timeStamp sourceOrNone outputKindList llvmCode = do
  case sourceOrNone of
    Right source -> do
      kindPathList <- zipWithM (Path.attachOutputPath target) outputKindList (repeat source)
      forM_ kindPathList $ \(_, outputPath) -> Path.ensureDir $ parent outputPath
      emitAll clangOptions llvmCode kindPathList
      forM_ (map snd kindPathList) $ \path -> do
        Path.setModificationTime path timeStamp
    Left t -> do
      MainModule mainModule <- getMainModule
      kindPathList <- zipWithM (Path.getOutputPathForEntryPoint mainModule) outputKindList (repeat t)
      forM_ kindPathList $ \(_, path) -> Path.ensureDir $ parent path
      emitAll clangOptions llvmCode kindPathList
      forM_ (map snd kindPathList) $ \path -> do
        Path.setModificationTime path timeStamp

emitAll :: [ClangOption] -> LLVMCode -> [(OK.OutputKind, Path Abs File)] -> App ()
emitAll clangOptions llvmCode kindPathList = do
  case kindPathList of
    [] ->
      return ()
    (kind, path) : rest -> do
      emit' clangOptions llvmCode kind path
      emitAll clangOptions llvmCode rest

emit' :: [ClangOption] -> LLVMCode -> OK.OutputKind -> Path Abs File -> App ()
emit' clangOptString llvmCode kind path = do
  case kind of
    OK.LLVM -> do
      report $ "Saving: " <> T.pack (toFilePath path)
      Path.writeByteString path llvmCode
    OK.Object ->
      emitInner clangOptString llvmCode path

emitInner :: [ClangOption] -> L.ByteString -> Path Abs File -> App ()
emitInner additionalClangOptions llvm outputPath = do
  clang <- liftIO External.getClang
  let optionList = clangBaseOpt outputPath ++ additionalClangOptions
  let ProcessRunner.Runner {run10} = ProcessRunner.ioRunner
  let spec =
        ProcessRunner.Spec
          { cmdspec = RawCommand clang optionList,
            cwd = Nothing
          }
  report $ "Executing: " <> T.pack (show (clang, optionList))
  value <- liftIO $ run10 spec (ProcessRunner.Lazy llvm)
  case value of
    Right _ ->
      return ()
    Left err ->
      Throw.throw $ ProcessRunner.toCompilerError err

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

link :: [String] -> [Path Abs File] -> Path Abs File -> App ()
link clangOptions objectPathList outputPath = do
  clang <- liftIO External.getClang
  ensureDir $ parent outputPath
  External.run clang $ clangLinkOpt objectPathList outputPath (unwords clangOptions)

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
