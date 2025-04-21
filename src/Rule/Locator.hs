module Rule.Locator
  ( Locator,
    reify,
  )
where

import Data.Text qualified as T
import Rule.Const
import Rule.GlobalLocator qualified as GL
import Rule.LocalLocator qualified as LL

type Locator =
  (GL.GlobalLocator, LL.LocalLocator)

reify :: Locator -> T.Text
reify (gl, ll) =
  GL.reify gl <> nsSep <> LL.reify ll
