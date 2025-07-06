module Language.RawTerm.Locator
  ( Locator,
    reify,
  )
where

import Data.Text qualified as T
import Language.Common.Const
import Language.Common.GlobalLocator qualified as GL
import Language.Common.LocalLocator qualified as LL

type Locator =
  (GL.GlobalLocator, LL.LocalLocator)

reify :: Locator -> T.Text
reify (gl, ll) =
  GL.reify gl <> nsSep <> LL.reify ll
