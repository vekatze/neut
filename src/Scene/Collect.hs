module Scene.Collect where

import Context.Env qualified as Env
import Data.Maybe
import Entity.Module
import Entity.Target
import Prelude hiding (log)

class (Env.Context m) => Context m

collectTargetList :: Context m => Maybe Target -> m [Target]
collectTargetList mTarget = do
  flip getTargetList mTarget <$> Env.getMainModule
