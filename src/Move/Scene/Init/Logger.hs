module Move.Scene.Init.Logger
  ( Handle,
    new,
    initializeLogger,
  )
where

import Move.Console.Report qualified as Report
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.Env qualified as Env
import Move.Scene.Init.Base qualified as Base
import Rule.Config.Remark qualified as Remark

data Handle
  = Handle
  { colorHandle :: Color.Handle,
    reportHandle :: Report.Handle,
    envHandle :: Env.Handle,
    debugHandle :: Debug.Handle
  }

new :: Base.Handle -> Handle
new (Base.Handle {..}) = do
  Handle {..}

initializeLogger :: Handle -> Remark.Config -> IO ()
initializeLogger _ _ = do
  return ()
