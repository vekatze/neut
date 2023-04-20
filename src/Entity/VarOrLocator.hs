module Entity.VarOrLocator where

import Data.Text qualified as T
import Entity.GlobalLocator qualified as GL
import Entity.LocalLocator qualified as LL

data VarOrLocator
  = Var T.Text
  | Locator GL.GlobalLocator LL.LocalLocator
