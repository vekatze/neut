module Console.Handle
  ( Handle (..),
    stdoutSupportsANSI,
    stderrSupportsANSI,
    shouldColorizeStdout,
    shouldColorizeStderr,
    supportsInteractiveOutput,
  )
where

data Handle = InternalHandle
  { _stdoutIsTTY :: Bool,
    _stderrIsTTY :: Bool,
    _termIsDumb :: Bool,
    _shouldColorizeStdoutByUser :: Bool,
    _shouldColorizeStderrByUser :: Bool
  }

stdoutSupportsANSI :: Handle -> Bool
stdoutSupportsANSI h = do
  _stdoutIsTTY h && not (_termIsDumb h)

stderrSupportsANSI :: Handle -> Bool
stderrSupportsANSI h = do
  _stderrIsTTY h && not (_termIsDumb h)

shouldColorizeStdout :: Handle -> Bool
shouldColorizeStdout h = do
  _shouldColorizeStdoutByUser h && stdoutSupportsANSI h

shouldColorizeStderr :: Handle -> Bool
shouldColorizeStderr h = do
  _shouldColorizeStderrByUser h && stderrSupportsANSI h

supportsInteractiveOutput :: Handle -> Bool
supportsInteractiveOutput h = do
  stdoutSupportsANSI h && stderrSupportsANSI h
