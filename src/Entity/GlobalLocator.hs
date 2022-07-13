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
      extract alias <> nsSep <> SL.toText locator
    GlobalLocatorAlias alias ->
      BN.reify $ GLA.reify alias

reflect' :: Throw.Context -> T.Text -> IO (ModuleAlias, SL.SourceLocator)
reflect' ctx rawTxt = do
  case T.breakOn nsSep rawTxt of
    (_, "") ->
      Throw.raiseError' ctx $
        "couldn't parse given global locator: " <> rawTxt
    (prefix, suffix) -> do
      locator <- SL.reflect $ T.tail suffix
      return (ModuleAlias prefix, locator)

reflect :: Throw.Context -> H.Hint -> T.Text -> IO GlobalLocator
reflect ctx m rawTxt = do
  case T.breakOn nsSep rawTxt of
    (_, "") ->
      return $ GlobalLocatorAlias (GLA.GlobalLocatorAlias (BN.fromText rawTxt)) -- rawTxt doesn't contain any nsSeps
    (prefix, suffix) -> do
      case SL.reflect $ T.tail suffix of
        Just locator ->
          return (GlobalLocator (ModuleAlias prefix) locator)
        Nothing ->
          Throw.raiseError ctx m $ "global locators can't end with '" <> nsSep <> "'"
