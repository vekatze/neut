module Language.RawTerm.Rule.Locator
  ( Locator,
    reify,
  )
where

import Data.Text qualified as T
import Language.Common.Rule.Const
import Language.Common.Rule.GlobalLocator qualified as GL
import Language.Common.Rule.LocalLocator qualified as LL

type Locator =
  (GL.GlobalLocator, LL.LocalLocator)

reify :: Locator -> T.Text
reify (gl, ll) =
  GL.reify gl <> nsSep <> LL.reify ll
