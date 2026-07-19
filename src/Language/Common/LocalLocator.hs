module Language.Common.LocalLocator
  ( LocalLocator (..),
    new,
    fromBaseNameList,
    extend,
    prepend,
    baseName,
    baseNameList,
    reify,
    reflect,
  )
where

import App.Error
import Control.Monad
import Data.Binary
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import GHC.Generics
import Language.Common.BaseName qualified as BN
import Language.Common.Const (nsSep)
import Language.Common.NamePath qualified as NP
import Logger.Hint qualified as H
import Prelude hiding (length)

-- the in-file body path of a name: namespace segments plus the final name
newtype LocalLocator = MakeLocalLocator
  { bodyPath :: NE.NonEmpty BN.BaseName
  }
  deriving (Generic, Show, Eq, Ord)

instance Binary LocalLocator

instance Hashable LocalLocator

reify :: LocalLocator -> T.Text
reify ll =
  T.intercalate nsSep $ map BN.reify $ NE.toList $ bodyPath ll

reflect :: H.Hint -> T.Text -> Either Error LocalLocator
reflect m rawTxt = do
  let segments = T.splitOn nsSep rawTxt
  when (any T.null segments) $ do
    Left $ newError m $ "Invalid name: " <> rawTxt
  bns <- NP.normalize <$> mapM (BN.reflect m) segments
  case bns of
    [] ->
      Left $ newError m $ "Invalid name: " <> rawTxt
    b : bs ->
      return $ MakeLocalLocator {bodyPath = b :| bs}

new :: BN.BaseName -> LocalLocator
new base =
  MakeLocalLocator {bodyPath = base :| []}

fromBaseNameList :: BN.BaseName -> [BN.BaseName] -> LocalLocator
fromBaseNameList b bs =
  MakeLocalLocator {bodyPath = b :| bs}

extend :: LocalLocator -> [BN.BaseName] -> LocalLocator
extend ll rest =
  MakeLocalLocator {bodyPath = NE.appendList (bodyPath ll) rest}

prepend :: [BN.BaseName] -> LocalLocator -> LocalLocator
prepend prefix ll =
  MakeLocalLocator {bodyPath = NE.prependList prefix (bodyPath ll)}

baseName :: LocalLocator -> BN.BaseName
baseName ll =
  NE.last (bodyPath ll)

baseNameList :: LocalLocator -> [BN.BaseName]
baseNameList ll =
  NE.toList (bodyPath ll)
