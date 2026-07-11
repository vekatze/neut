module ProgressIndicator.ShowProgress
  ( Handle,
    new,
    increment,
    close,
  )
where

import Console.Handle qualified as Console
import Console.Print qualified as Console
import Console.Text qualified as Console
import Control.Concurrent.MVar
import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Logger.Debug qualified as Logger
import Logger.Handle qualified as Logger
import ProgressIndicator.Handle
import ProgressIndicator.ProgressIndicator
import System.Console.ANSI
import System.IO hiding (Handle)
import UnliftIO.Async
import UnliftIO.Concurrent (threadDelay)

new :: Console.Handle -> Logger.Handle -> Maybe Int -> T.Text -> T.Text -> [SGR] -> IO Handle
new consoleHandle loggerHandle numOfItems workingTitle completedTitle color = do
  let reportMode = Console.getReportMode consoleHandle
  case (reportMode, numOfItems) of
    (Console.NoReport, _) ->
      return Nothing
    (Console.TraceReport _, _) ->
      return Nothing
    (_, Just i)
      | i <= 0 ->
          return Nothing
    _ -> do
      progress <-
        case numOfItems of
          Nothing ->
            return Nothing
          Just v -> do
            return $ Just (0, v)
      let progressBar = ProgressBar {workingTitle, completedTitle, color, progress}
      progressBarRef <- newIORef progressBar
      printLock <- newMVar ()
      case reportMode of
        Console.PlainReport -> do
          Console.printStdErr consoleHandle $ Console.pack' workingTitle <> Console.pack' "\n"
          return $ Just $ Handle {consoleHandle, loggerHandle, progressBarRef, printLock, reportMode, renderThread = Nothing}
        Console.FancyReport -> do
          renderThread <- Just <$> async (render consoleHandle 0 progressBarRef)
          return $ Just $ Handle {consoleHandle, loggerHandle, progressBarRef, printLock, reportMode, renderThread}

increment :: Handle -> T.Text -> IO ()
increment mh label = do
  case mh of
    Nothing ->
      return ()
    Just h -> do
      withMVar (printLock h) $ \_ -> do
        progressBar <-
          atomicModifyIORef' (progressBarRef h) $ \progressBar -> do
            let progressBar' = next progressBar
            (progressBar', progressBar')
        case reportMode h of
          Console.PlainReport ->
            printPlainProgress h label progressBar
          _ ->
            when (Console.isActivityMode $ consoleHandle h) $
              Logger.report (loggerHandle h) $ "Compiled: " <> label

printPlainProgress :: InnerHandle -> T.Text -> ProgressBar -> IO ()
printPlainProgress h label progressBar = do
  case progress progressBar of
    Nothing ->
      return ()
    Just (current, size) -> do
      let sizeText = T.pack (show size)
      let currentText = padLeft (T.length sizeText) ' ' $ T.pack (show current)
      Console.printStdErr (consoleHandle h) $
        Console.pack' "["
          <> Console.pack' currentText
          <> Console.pack' "/"
          <> Console.pack' sizeText
          <> Console.pack' "] "
          <> Console.pack' label
          <> Console.pack' "\n"

padLeft :: Int -> Char -> T.Text -> T.Text
padLeft width c text = do
  let paddingWidth = max 0 $ width - T.length text
  T.replicate paddingWidth (T.singleton c) <> text

render :: Console.Handle -> Frame -> IORef ProgressBar -> IO ()
render consoleHandle i ref = do
  progressBar <- readIORef ref
  Console.printStdErr consoleHandle $ renderInProgress i progressBar <> Console.pack' "\n"
  threadDelay 33333 -- 2F
  clear ref
  render consoleHandle (i + 1) ref

clear :: IORef ProgressBar -> IO ()
clear ref = do
  hSetCursorColumn stderr 0
  hClearFromCursorToLineEnd stderr
  hCursorUpLine stderr 1
  hClearFromCursorToLineEnd stderr
  progressBar <- readIORef ref
  case progress progressBar of
    Nothing ->
      return ()
    Just _ -> do
      hCursorUpLine stderr 1
      hClearFromCursorToLineEnd stderr

close :: Handle -> IO ()
close mh = do
  case mh of
    Nothing ->
      return ()
    Just h -> do
      case renderThread h of
        Nothing ->
          return ()
        Just thread -> do
          cancel thread
          clear (progressBarRef h)
      progressBar <- readIORef (progressBarRef h)
      case reportMode h of
        Console.PlainReport ->
          Console.printStdErr (consoleHandle h) $ Console.pack' (completedTitle progressBar) <> Console.pack' "\n"
        Console.FancyReport ->
          Console.printStdErr (consoleHandle h) $ renderFinished progressBar
        _ ->
          return ()
