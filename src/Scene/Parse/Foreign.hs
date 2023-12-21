module Scene.Parse.Foreign
  ( activateForeign,
    interpretForeign,
  )
where

import Context.App
import Context.Decl qualified as Decl
import Control.Monad
import Entity.DeclarationName qualified as DN
import Entity.Foreign qualified as F
import Entity.RawProgram
import Entity.Syntax.Series qualified as SE

activateForeign :: [F.Foreign] -> App ()
activateForeign foreignItemList = do
  forM_ foreignItemList $ \(F.Foreign name domList cod) -> do
    Decl.insDeclEnv' (DN.Ext name) domList cod

interpretForeign :: Maybe RawForeign -> App [F.Foreign]
interpretForeign foreignOrNone = do
  case foreignOrNone of
    Nothing ->
      return []
    Just (RawForeign _ foreignItemList) -> do
      return $ map interpretForeignItem $ SE.extract foreignItemList

interpretForeignItem :: RawForeignItem -> F.Foreign
interpretForeignItem (RawForeignItem name _ lts _ _ cod) =
  F.Foreign name (SE.extract lts) cod
