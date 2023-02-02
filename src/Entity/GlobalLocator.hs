module Entity.GlobalLocator where

import qualified Context.Throw as Throw
import Data.Binary
import Data.Hashable
import qualified Data.Text as T
import qualified Entity.BaseName as BN
import Entity.Const
import qualified Entity.GlobalLocatorAlias as GLA
import qualified Entity.Hint as H
import Entity.ModuleAlias
import qualified Entity.SourceLocator as SL
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
