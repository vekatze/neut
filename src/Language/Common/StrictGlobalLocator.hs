module Language.Common.StrictGlobalLocator
  ( StrictGlobalLocator,
    moduleID,
    sourceLocator,
    sourceSegments,
    new,
    reify,
    llvmGlobalLocator,
    baseGlobalLocatorOf,
  )
where

import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import GHC.Generics
import Language.Common.BaseName qualified as BN
import Language.Common.Const
import Language.Common.ModuleDigest qualified as MD
import Language.Common.ModuleID qualified as MID
import Language.Common.SourceLocator qualified as SL

-- the last two fields cache the textual form and the source path segments;
-- both are recomputed from the source locator on every name lookup otherwise.
data StrictGlobalLocator = StrictGlobalLocator
  { moduleID :: MID.ModuleID,
    sourceLocator :: SL.SourceLocator,
    _reifiedText :: T.Text,
    sourceSegments :: [BN.BaseName]
  }
  deriving (Generic, Show)

instance Eq StrictGlobalLocator where
  gl1 == gl2 = reify gl1 == reify gl2

instance Binary StrictGlobalLocator where
  put gl = do
    put $ moduleID gl
    put $ sourceLocator gl
  get =
    new <$> get <*> get

instance Hashable StrictGlobalLocator where
  hashWithSalt s gl = hashWithSalt s (reify gl)

new :: MID.ModuleID -> SL.SourceLocator -> StrictGlobalLocator
new mid sl = do
  let modulePathText = case mid of
        MID.Main -> "this"
        MID.Base -> "base"
        MID.Library digest -> MD.reify digest
  StrictGlobalLocator
    { moduleID = mid,
      sourceLocator = sl,
      _reifiedText = modulePathText <> doubleColon <> SL.toText sl,
      sourceSegments = SL.toBaseNameList sl
    }

reify :: StrictGlobalLocator -> T.Text
reify =
  _reifiedText

llvmGlobalLocator :: StrictGlobalLocator
llvmGlobalLocator =
  new MID.Base SL.llvmLocator

baseGlobalLocatorOf :: SL.SourceLocator -> StrictGlobalLocator
baseGlobalLocatorOf =
  new MID.Base
