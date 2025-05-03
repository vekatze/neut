module Main.Move.Scene.Parse.Discern.Noema
  ( castToNoemaIfNecessary,
    castFromNoemaIfNecessary,
  )
where

import Control.Comonad.Cofree hiding (section)
import Main.Move.Language.Utility.Gensym qualified as Gensym
import Main.Move.Scene.Parse.Discern.Handle qualified as H
import Main.Rule.Magic qualified as M
import Main.Rule.Noema qualified as N
import Main.Rule.WeakTerm qualified as WT

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
  t <- Gensym.newHole (H.gensymHandle h) m []
  return $ m :< WT.Magic (M.WeakMagic $ M.Cast t (m :< WT.BoxNoema t) e)

castFromNoema :: H.Handle -> WT.WeakTerm -> IO WT.WeakTerm
castFromNoema h e@(m :< _) = do
  t <- Gensym.newHole (H.gensymHandle h) m []
  return $ m :< WT.Magic (M.WeakMagic $ M.Cast (m :< WT.BoxNoema t) t e)
