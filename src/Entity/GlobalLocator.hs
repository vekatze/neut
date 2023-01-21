module Entity.GlobalLocator where

import Context.Throw qualified as Throw
import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Const
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

reflect :: Throw.Context m => H.Hint -> T.Text -> m GlobalLocator
reflect m rawTxt = do
  baseNameList <- BN.bySplit m rawTxt
  case baseNameList of
    [baseName] ->
      return $ GlobalLocatorAlias (GLA.GlobalLocatorAlias baseName)
    prefix : rest
      | Just locator <- SL.fromBaseNameList rest ->
          return (GlobalLocator (ModuleAlias prefix) locator)
    _ ->
      Throw.raiseError m $ "invalid global locator: `" <> rawTxt <> "`"
