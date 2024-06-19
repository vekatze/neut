module Context.LLVM
  ( emit,
    link,
    ensureSetupSanity,
  )
where

import Context.App
import Context.Env (getMainModule)
import Context.External qualified as External
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Time.Clock
import Entity.Config.Build
import Entity.OutputKind qualified as OK
import Entity.Source
import Entity.Target
import GHC.IO.Handle
import Path
import Path.IO
import System.Process

type ClangOption = String

type LLVMCode = L.ByteString

ensureSetupSanity :: Config -> App ()
ensureSetupSanity cfg = do
  let willBuildObjects = OK.Object `elem` outputKindList cfg
  let willLink = not $ shouldSkipLink cfg
  when (not willBuildObjects && willLink) $
    Throw.raiseError' "`--skip-link` must be set explicitly when `--emit` does not contain `object`"

emit :: [ClangOption] -> UTCTime -> Either MainTarget Source -> [OK.OutputKind] -> L.ByteString -> App ()
emit clangOptions timeStamp sourceOrNone outputKindList llvmCode = do
  case sourceOrNone of
    Right source -> do
      kindPathList <- zipWithM Path.attachOutputPath outputKindList (repeat source)
      forM_ kindPathList $ \(_, outputPath) -> Path.ensureDir $ parent outputPath
      emitAll clangOptions llvmCode kindPathList
      forM_ (map snd kindPathList) $ \path -> do
        Path.setModificationTime path timeStamp
    Left t -> do
      mainModule <- getMainModule
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
      Path.writeByteString path llvmCode
    OK.Object ->
      emitInner clangOptString llvmCode path

emitInner :: [ClangOption] -> L.ByteString -> Path Abs File -> App ()
emitInner additionalClangOptions llvm outputPath = do
  clang <- liftIO External.getClang
  let clangCmd = proc clang (clangBaseOpt outputPath ++ additionalClangOptions)
  withRunInIO $ \runInIO ->
    withCreateProcess clangCmd {std_in = CreatePipe, std_err = CreatePipe} $
      \mStdin _ mClangErrorHandler clangProcessHandler -> do
        case (mStdin, mClangErrorHandler) of
          (Just stdin, Just clangErrorHandler) -> do
            L.hPut stdin llvm
            hClose stdin
            clangExitCode <- waitForProcess clangProcessHandler
            runInIO $ External.raiseIfProcessFailed (T.pack clang) clangExitCode clangErrorHandler
          (Nothing, _) ->
            runInIO $ Throw.raiseError' "Could not obtain stdin"
          (_, Nothing) ->
            runInIO $ Throw.raiseError' "Could not obtain stderr"

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
    ++ pathList
    ++ words additionalOptionStr
