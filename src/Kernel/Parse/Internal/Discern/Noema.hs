module Kernel.Parse.Internal.Discern.Noema
  ( castToNoemaIfNecessary,
    castFromNoemaIfNecessary,
  )
where

import Control.Comonad.Cofree hiding (section)
import Kernel.Parse.Internal.Discern.Handle qualified as H
import Language.Common.Magic qualified as M
import Language.Common.Noema qualified as N
import Language.WeakTerm.CreateHole qualified as WT
import Language.WeakTerm.WeakTerm qualified as WT

castToNoemaIfNecessary :: H.Handle -> N.IsNoetic -> WT.WeakTerm -> IO WT.WeakTerm
castToNoemaIfNecessary h isNoetic e =
  if isNoetic
    then castToNoema h e
    else return e

castFromNoemaIfNecessary :: H.Handle -> N.IsNoetic -> WT.WeakTerm -> IO WT.WeakTerm
castFromNoemaIfNecessary h isNoetic e =
  if isNoetic
    then castFromNoema h e
    else return e

castToNoema :: H.Handle -> WT.WeakTerm -> IO WT.WeakTerm
castToNoema h e@(m :< _) = do
  t <- WT.createHole (H.gensymHandle h) m []
  return $ m :< WT.Magic (M.WeakMagic $ M.Cast t (m :< WT.BoxNoema t) e)

castFromNoema :: H.Handle -> WT.WeakTerm -> IO WT.WeakTerm
castFromNoema h e@(m :< _) = do
  t <- WT.createHole (H.gensymHandle h) m []
  return $ m :< WT.Magic (M.WeakMagic $ M.Cast (m :< WT.BoxNoema t) t e)
