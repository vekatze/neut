module Move.Scene.Init.Logger
  ( Handle,
    new,
    initializeLogger,
  )
where

import Move.Console.Report qualified as Report
import Move.Context.App
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.Env qualified as Env
import Rule.Config.Remark qualified as Remark

data Handle
  = Handle
  { colorHandle :: Color.Handle,
    reportHandle :: Report.Handle,
    envHandle :: Env.Handle,
    debugHandle :: Debug.Handle
  }

new :: App Handle
new = do
  colorHandle <- Color.new
  reportHandle <- Report.new
  envHandle <- Env.new
  debugHandle <- Debug.new
  return $ Handle {..}

initializeLogger :: Handle -> Remark.Config -> IO ()
initializeLogger h cfg = do
  Color.setShouldColorizeStdout (colorHandle h) $ Remark.shouldColorize cfg
  Color.setShouldColorizeStderr (colorHandle h) $ Remark.shouldColorize cfg
  Report.setEndOfEntry (reportHandle h) $ Remark.endOfEntry cfg
  Env.setSilentMode (envHandle h) $ Remark.enableSilentMode cfg
  Debug.setDebugMode (debugHandle h) $ Remark.enableDebugMode cfg
