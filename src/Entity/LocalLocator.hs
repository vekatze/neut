module Entity.LocalLocator
  ( LocalLocator (..),
    new,
    reify,
    reflect,
    extend,
  )
where

import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Hint qualified as H
import Entity.Log
import GHC.Generics

newtype LocalLocator = MakeLocalLocator
  { baseName :: BN.BaseName
  }
  deriving (Generic, Show, Eq)

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
