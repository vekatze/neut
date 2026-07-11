module Language.Common.StrictGlobalLocator
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
import Language.Common.Const
import Language.Common.ModuleDigest qualified as MD
import Language.Common.ModuleID qualified as MID
import Language.Common.SourceLocator qualified as SL

data StrictGlobalLocator = StrictGlobalLocator
  { moduleID :: MID.ModuleID,
    sourceLocator :: SL.SourceLocator
  }
  deriving (Generic, Eq, Show)

instance Binary StrictGlobalLocator

instance Hashable StrictGlobalLocator

reify :: StrictGlobalLocator -> T.Text
reify gl = do
  let route = case moduleID gl of
        MID.Main -> ""
        MID.Base -> "base"
        MID.Library digest -> MD.reify digest
  route <> routeSep <> SL.toText (sourceLocator gl)

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
