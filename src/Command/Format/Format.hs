module Command.Format.Format
  ( Handle,
    new,
    format,
  )
where

import App.App (App)
import Command.Common.Format qualified as Format
import CommandParser.Config.Format
import Console.FormatMode (FormatMode (..))
import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Kernel.Common.Const (ensFileExtension, sourceFileExtension)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Module (extractModule, getRelativePathFromModuleRoot)
import Path
import Path.Collect (collectFilePathList, hasExtension)
import Path.IO (resolveFile')
import Path.Read (readTextFromPath, readTextFromStdin)
import Path.Write
import System.Exit (ExitCode (ExitFailure), exitWith)

newtype Handle = Handle
  { globalHandle :: Global.Handle
  }

new :: Global.Handle -> Handle
new globalHandle = do
  Handle {..}

format :: Handle -> Config -> App ()
format h cfg = do
  pathList <- getTargetPathList cfg
  stdinPath <- mapM resolvePath (stdinFilePath cfg)
  let formatHandle = Format.new (globalHandle h)
  needsFormattingList <- forM (catMaybes [stdinPath] ++ pathList) $ \path -> do
    let isStdinTarget = Just path == stdinPath
    content <- readTextFromPathOrStdin isStdinTarget (inject path)
    content' <- case path of
      NeutPath path' ->
        Format.formatSource formatHandle (shouldMinimizeImports cfg) path' content
      EnsPath path' ->
        Format.formatEns path' content
    needsFormatting <- liftIO $ applyFormatMode isStdinTarget (formatMode cfg) (inject path) content content'
    return $ if needsFormatting then Just path else Nothing
  when (formatMode cfg == Check) $ do
    let changedPathList = catMaybes needsFormattingList
    unless (null changedPathList) $ do
      let mainModule = extractModule $ Env.getMainModule (Global.envHandle (globalHandle h))
      liftIO $ do
        mapM_ (TIO.putStrLn . getRelativePathFromModuleRoot mainModule . inject) changedPathList
        exitWith (ExitFailure 1)

data ItemPath
  = NeutPath (Path Abs File)
  | EnsPath (Path Abs File)
  deriving (Eq)

inject :: ItemPath -> Path Abs File
inject p =
  case p of
    NeutPath p' ->
      p'
    EnsPath p' ->
      p'

getTargetPathList :: Config -> App [ItemPath]
getTargetPathList cfg = do
  pathList <- collectFilePathList isFormatTarget (filePathStringList cfg)
  return $ map toItemPath pathList

resolvePath :: FilePath -> App ItemPath
resolvePath pathString = do
  toItemPath <$> resolveFile' pathString

toItemPath :: Path Abs File -> ItemPath
toItemPath path =
  if hasExtension ensFileExtension path
    then EnsPath path
    else NeutPath path

isFormatTarget :: Path Abs File -> Bool
isFormatTarget path =
  hasExtension sourceFileExtension path || hasExtension ensFileExtension path

readTextFromPathOrStdin :: Bool -> Path Abs File -> App T.Text
readTextFromPathOrStdin isStdinTarget path = do
  if isStdinTarget
    then readTextFromStdin
    else readTextFromPath path

applyFormatMode :: Bool -> FormatMode -> Path Abs File -> T.Text -> T.Text -> IO Bool
applyFormatMode isStdinTarget mode path content content' =
  case mode of
    Check ->
      return $ content /= content'
    Stdout -> do
      printText content'
      return False
    Write -> do
      if isStdinTarget
        then printText content'
        else writeText path content'
      return False
