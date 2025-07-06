module Color.CreateHandle (createHandle) where

import Color.Handle
import System.IO hiding (Handle)

createHandle :: Bool -> Bool -> IO Handle
createHandle shouldColorizeStdout shouldColorizeStderr = do
  stdoutIsTerminal <- hIsTerminalDevice stdout
  stderrIsTerminal <- hIsTerminalDevice stderr
  let _shouldColorizeStdout = shouldColorizeStdout && stdoutIsTerminal
  let _shouldColorizeStderr = shouldColorizeStderr && stderrIsTerminal
  return $ InternalHandle {..}
