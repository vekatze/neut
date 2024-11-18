module Scene.Parse.Foreign (interpretForeign) where

import Context.App
import Entity.Foreign qualified as F
import Entity.RawProgram
import Entity.Syntax.Series qualified as SE
import Entity.WeakTerm qualified as WT

interpretForeign :: [RawForeignItemF WT.WeakTerm] -> App [F.WeakForeign]
interpretForeign foreignItemList = do
  mapM interpretForeignItem foreignItemList

interpretForeignItem :: RawForeignItemF WT.WeakTerm -> App F.WeakForeign
interpretForeignItem (RawForeignItemF m name _ lts _ _ cod) = do
  let lts' = SE.extract lts
  return $ F.Foreign m name lts' cod
