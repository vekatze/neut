module Entity.StrictGlobalLocator where

import Data.Binary
import Data.Hashable
import qualified Data.Text as T
import Entity.Const
import qualified Entity.ModuleID as MID
import qualified Entity.SourceLocator as SL
import GHC.Generics

data StrictGlobalLocator = StrictGlobalLocator
  { moduleID :: MID.ModuleID,
    sourceLocator :: SL.SourceLocator
  }
  deriving (Generic, Eq, Show)

instance Binary StrictGlobalLocator

instance Hashable StrictGlobalLocator

reify :: StrictGlobalLocator -> T.Text
reify gl =
  MID.reify (moduleID gl) <> nsSep <> SL.reify (sourceLocator gl)

llvmGlobalLocator :: StrictGlobalLocator
llvmGlobalLocator =
  StrictGlobalLocator
    { moduleID = MID.Base,
      sourceLocator = SL.llvmLocator
    }

baseGlobalLocatorOf :: SL.SourceLocator -> StrictGlobalLocator
baseGlobalLocatorOf sl =
  StrictGlobalLocator
    { moduleID = MID.Base,
      sourceLocator = sl
    }
