module Entity.StrictGlobalLocator where

import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import Entity.Const
import Entity.ModuleID qualified as MID
import Entity.SourceLocator qualified as SL
import GHC.Generics
import Path

data StrictGlobalLocator = StrictGlobalLocator
  { moduleID :: MID.ModuleID,
    sourceLocator :: SL.SourceLocator
  }
  deriving (Generic, Eq, Show)

instance Binary StrictGlobalLocator

instance Hashable StrictGlobalLocator

reify :: StrictGlobalLocator -> T.Text
reify gl =
  MID.reify (moduleID gl) <> nsSep <> SL.toText (sourceLocator gl)

llvmGlobalLocator :: StrictGlobalLocator
llvmGlobalLocator =
  StrictGlobalLocator
    { moduleID = MID.Base,
      sourceLocator = SL.llvmLocator
    }

natGlobalLocator :: StrictGlobalLocator
natGlobalLocator =
  StrictGlobalLocator
    { moduleID = MID.Base,
      sourceLocator = SL.natLocator
    }

baseGlobalLocatorOf :: SL.SourceLocator -> StrictGlobalLocator
baseGlobalLocatorOf sl =
  StrictGlobalLocator
    { moduleID = MID.Base,
      sourceLocator = sl
    }

getRelPathText :: StrictGlobalLocator -> T.Text
getRelPathText sgl =
  T.pack $ toFilePath $ SL.reify $ sourceLocator sgl
