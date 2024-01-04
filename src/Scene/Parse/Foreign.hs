module Scene.Parse.Foreign
  ( activateForeign,
    interpretForeign,
  )
where

import Context.App
import Context.Decl qualified as Decl
import Context.Env qualified as Env
import Context.Throw qualified as Throw
import Control.Monad
import Entity.DataSize qualified as DS
import Entity.DeclarationName qualified as DN
import Entity.Foreign qualified as F
import Entity.Hint
import Entity.LowType qualified as LT
import Entity.LowType.FromRawLowType
import Entity.RawLowType qualified as RLT
import Entity.RawProgram
import Entity.Syntax.Series qualified as SE

activateForeign :: [F.Foreign] -> App ()
activateForeign foreignItemList = do
  forM_ foreignItemList $ \(F.Foreign name domList cod) -> do
    Decl.insDeclEnv' (DN.Ext name) domList cod

interpretForeign :: SE.Series RawForeignItem -> App [F.Foreign]
interpretForeign foreignItemList = do
  dataSize <- Env.getDataSize'
  mapM (interpretForeignItem dataSize) $ SE.extract foreignItemList

interpretForeignItem :: DS.DataSize -> RawForeignItem -> App F.Foreign
interpretForeignItem dataSize (RawForeignItem m name _ lts _ _ cod) = do
  lts' <- mapM (fromRawLowType' m dataSize) $ SE.extract lts
  cod' <- fromRawLowType' m dataSize cod
  return $ F.Foreign name lts' cod'

fromRawLowType' :: Hint -> DS.DataSize -> RLT.RawLowType -> App LT.LowType
fromRawLowType' m dataSize rawLowType =
  case fromRawLowType dataSize rawLowType of
    Left err ->
      Throw.raiseError m err
    Right value ->
      return value
