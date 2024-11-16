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

interpretForeign :: SE.Series RawForeignItem -> App [F.Foreign]
interpretForeign foreignItemList = do
  mapM interpretForeignItem $ SE.extract foreignItemList

interpretForeignItem :: RawForeignItem -> App F.Foreign
interpretForeignItem (RawForeignItem _ name _ lts _ _ cod) = do
  let lts' = SE.extract lts
  return $ F.Foreign name lts' cod
