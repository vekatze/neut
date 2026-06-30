module Console.CreateHandle (createHandle) where

import Console.Handle
import System.Environment (lookupEnv)
import System.IO hiding (Handle)

createHandle :: Bool -> Bool -> ReportMode -> IO Handle
createHandle _shouldColorizeStdoutByUser _shouldColorizeStderrByUser _reportModeByUser = do
  _stdoutIsTTY <- hIsTerminalDevice stdout
  _stderrIsTTY <- hIsTerminalDevice stderr
  term <- lookupEnv "TERM"
  let _termIsDumb = term == Just "dumb"
  return $ InternalHandle {..}
