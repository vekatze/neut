module Language.Common.Rule.LocalLocator
  ( LocalLocator (..),
    new,
    reify,
    reflect,
    extend,
    length,
  )
where

import Aux.Error.Rule.Error
import Aux.Logger.Rule.Hint qualified as H
import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import GHC.Generics
import Language.Common.Rule.BaseName qualified as BN
import Prelude hiding (length)

newtype LocalLocator = MakeLocalLocator
  { baseName :: BN.BaseName
  }
  deriving (Generic, Show, Eq, Ord)

instance Binary LocalLocator

instance Hashable LocalLocator

reify :: LocalLocator -> T.Text
reify ll =
  BN.reify $ baseName ll

reflect :: H.Hint -> T.Text -> Either Error LocalLocator
reflect m rawTxt = do
  bn <- BN.reflect m rawTxt
  return $ MakeLocalLocator {baseName = bn}

new :: BN.BaseName -> LocalLocator
new base =
  MakeLocalLocator {baseName = base}

extend :: LocalLocator -> BN.BaseName -> LocalLocator
extend base ext =
  MakeLocalLocator
    { baseName = BN.extend (baseName base) ext
    }

length :: LocalLocator -> Int
length MakeLocalLocator {baseName} =
  BN.length baseName
