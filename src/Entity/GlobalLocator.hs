module Entity.GlobalLocator where

import qualified Context.Throw as Throw
import Data.Binary
import Data.Hashable
import qualified Data.Text as T
import Entity.Const
import qualified Entity.GlobalLocatorAlias as GLA
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
      extract alias <> nsSep <> SL.reify locator
    GlobalLocatorAlias alias ->
      GLA.reify alias

-- extract (moduleAlias gl) <> nsSep <> SL.reify (sourceLocator gl)

reflect' :: Throw.Context -> T.Text -> IO (ModuleAlias, SL.SourceLocator)
reflect' ctx rawTxt = do
  case T.breakOn nsSep rawTxt of
    (_, "") ->
      Throw.raiseError' ctx $
        "couldn't parse given global locator: " <> rawTxt
    (prefix, suffix) -> do
      locator <- SL.reflect $ T.tail suffix
      return (ModuleAlias prefix, locator)

reflect :: T.Text -> IO GlobalLocator
reflect rawTxt = do
  case T.breakOn nsSep rawTxt of
    (_, "") ->
      return $ GlobalLocatorAlias (GLA.GlobalLocatorAlias rawTxt)
    (prefix, suffix) -> do
      locator <- SL.reflect $ T.tail suffix
      return (GlobalLocator (ModuleAlias prefix) locator)

-- GlobalLocator
--   { moduleAlias = ModuleAlias prefix,
--     sourceLocator = locator
--   }
