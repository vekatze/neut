module Main.Rule.Locator
  ( Locator,
    reify,
  )
where

import Data.Text qualified as T
import Main.Rule.Const
import Main.Rule.GlobalLocator qualified as GL
import Main.Rule.LocalLocator qualified as LL

type Locator =
  (GL.GlobalLocator, LL.LocalLocator)

reify :: Locator -> T.Text
reify (gl, ll) =
  GL.reify gl <> nsSep <> LL.reify ll
