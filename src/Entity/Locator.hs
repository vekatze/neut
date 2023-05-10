module Entity.Locator
  ( Locator,
    reify,
  )
where

import Data.Text qualified as T
import Entity.Const
import Entity.GlobalLocator qualified as GL
import Entity.LocalLocator qualified as LL

type Locator =
  (GL.GlobalLocator, LL.LocalLocator)

reify :: Locator -> T.Text
reify (gl, ll) =
  GL.reify gl <> nsSep <> LL.reify ll
