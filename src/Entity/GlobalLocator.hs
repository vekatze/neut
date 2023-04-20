module Entity.GlobalLocator where

import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.Error
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.Hint qualified as H
import Entity.ModuleAlias
import Entity.SourceLocator qualified as SL
import GHC.Generics

data GlobalLocator
  = GlobalLocator
      { moduleAlias :: ModuleAlias,
        sourceLocator :: SL.SourceLocator
      }
  | GlobalLocatorAlias GLA.GlobalLocatorAlias
  deriving (Generic, Show, Eq)

instance Binary GlobalLocator

instance Hashable GlobalLocator

reify :: GlobalLocator -> T.Text
reify gl =
  case gl of
    GlobalLocator alias locator ->
      BN.reify (extract alias) <> nsSep <> SL.toText locator
    GlobalLocatorAlias alias ->
      BN.reify $ GLA.reify alias

reflect :: H.Hint -> T.Text -> Either Error GlobalLocator
reflect m rawTxt = do
  baseNameList <- BN.bySplit m rawTxt
  case baseNameList of
    [baseName] ->
      return $ GlobalLocatorAlias (GLA.GlobalLocatorAlias baseName)
    prefix : rest
      | Just locator <- SL.fromBaseNameList rest ->
          return (GlobalLocator (ModuleAlias prefix) locator)
    _ ->
      Left $ newError m $ "invalid global locator: `" <> rawTxt <> "`"

reflectLocator :: H.Hint -> T.Text -> Either Error (ModuleAlias, SL.SourceLocator)
reflectLocator m rawTxt = do
  baseNameList <- BN.bySplit m rawTxt
  case baseNameList of
    [_] ->
      Left $ newError m $ "invalid source specifier: `" <> rawTxt <> "`"
    prefix : rest
      | Just locator <- SL.fromBaseNameList rest ->
          return (ModuleAlias prefix, locator)
    _ ->
      Left $ newError m $ "invalid global locator: `" <> rawTxt <> "`"
