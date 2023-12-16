module Scene.Parse.Foreign
  ( activateForeign,
    interpretForeign,
  )
where

import Context.App
import Context.Decl qualified as Decl
import Control.Monad
import Entity.C
import Entity.DeclarationName qualified as DN
import Entity.Foreign qualified as F
import Entity.RawProgram

activateForeign :: [F.Foreign] -> App ()
activateForeign foreignItemList = do
  forM_ foreignItemList $ \(F.Foreign name domList cod) -> do
    Decl.insDeclEnv' (DN.Ext name) domList cod

interpretForeign :: Maybe (RawForeign, C) -> App [F.Foreign]
interpretForeign foreignOrNone = do
  case foreignOrNone of
    Nothing ->
      return []
    Just (RawForeign _ (_, foreignItemList), _) -> do
      return $ map (interpretForeignItem . snd) foreignItemList

interpretForeignItem :: RawForeignItem -> F.Foreign
interpretForeignItem (RawForeignItem name _ lts _ (cod, _)) =
  F.Foreign name (map fst $ distillArgList lts) cod
