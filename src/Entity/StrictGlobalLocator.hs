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
    sourceLocator :: SL.SourceLocator,
    isPublic :: Bool
  }
  deriving (Generic, Eq, Show)

instance Binary StrictGlobalLocator

instance Hashable StrictGlobalLocator

reify :: StrictGlobalLocator -> T.Text
reify gl =
  MID.reify (moduleID gl) <> nsSep <> SL.toText (sourceLocator gl)

makePrivate :: StrictGlobalLocator -> StrictGlobalLocator
makePrivate sgl =
  sgl {isPublic = False}

llvmGlobalLocator :: StrictGlobalLocator
llvmGlobalLocator =
  StrictGlobalLocator
    { moduleID = MID.Base,
      sourceLocator = SL.llvmLocator,
      isPublic = True
    }

baseGlobalLocatorOf :: SL.SourceLocator -> StrictGlobalLocator
baseGlobalLocatorOf sl =
  StrictGlobalLocator
    { moduleID = MID.Base,
      sourceLocator = sl,
      isPublic = True
    }

getRelPathText :: StrictGlobalLocator -> T.Text
getRelPathText sgl =
  T.pack $ toFilePath $ SL.reify $ sourceLocator sgl
