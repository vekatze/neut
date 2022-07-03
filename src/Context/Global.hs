module Context.Global where

import qualified Context.Throw as Throw
import qualified Data.Text as T
import Entity.Hint

type GlobalVarName = T.Text

data Axis = Axis
  { register :: Hint -> GlobalVarName -> IO (),
    isDefined :: GlobalVarName -> IO Bool
  }

newtype Config = Config
  { throwCtx :: Throw.Context
  }
