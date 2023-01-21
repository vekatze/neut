module Entity.StrictGlobalLocator where

import Control.Monad.Catch
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

baseGlobalLocatorOf :: SL.SourceLocator -> StrictGlobalLocator
baseGlobalLocatorOf sl =
  StrictGlobalLocator
    { moduleID = MID.Base,
      sourceLocator = sl
    }

reflectInMainModule :: MonadThrow m => String -> m StrictGlobalLocator
reflectInMainModule relFilePathString = do
  filePath <- parseRelFile relFilePathString
  return $
    StrictGlobalLocator
      { moduleID = MID.Main,
        sourceLocator = SL.SourceLocator filePath
      }

getRelPathText :: StrictGlobalLocator -> T.Text
getRelPathText sgl =
  T.pack $ toFilePath $ SL.reify $ sourceLocator sgl
