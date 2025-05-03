module Main.Rule.StrictGlobalLocator
  ( StrictGlobalLocator (..),
    reify,
    llvmGlobalLocator,
    baseGlobalLocatorOf,
  )
where

import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import GHC.Generics
import Main.Rule.Const
import Main.Rule.ModuleID qualified as MID
import Main.Rule.SourceLocator qualified as SL

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
