module Move.Scene.Parse.Foreign (interpretForeign) where

import Move.Context.App
import Move.Context.Decl qualified as Decl
import Move.Context.Tag qualified as Tag
import Rule.Foreign qualified as F
import Rule.RawProgram
import Rule.Syntax.Series qualified as SE
import Rule.WeakTerm qualified as WT

interpretForeign :: [RawForeignItemF WT.WeakTerm] -> App [F.WeakForeign]
interpretForeign foreignItemList = do
  mapM interpretForeignItem foreignItemList

interpretForeignItem :: RawForeignItemF WT.WeakTerm -> App F.WeakForeign
interpretForeignItem (RawForeignItemF m name _ lts _ _ cod) = do
  let lts' = SE.extract lts
  Tag.insertExternalName m name m
  Decl.insPreDeclEnv name m
  return $ F.Foreign m name lts' cod
