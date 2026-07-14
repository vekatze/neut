module Kernel.Common.TopNameMap
  ( TopNameInfo,
    TopNameMap,
    LookupResult (..),
    lookupAvailable,
  )
where

import Data.HashMap.Strict qualified as Map
import Kernel.Common.GlobalName qualified as GN
import Language.Common.Availability qualified as AV
import Language.Common.BaseName qualified as BN
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.NominalTag qualified as NT
import Language.Common.StrictGlobalLocator qualified as SGL
import Logger.Hint

type TopNameInfo =
  (Hint, Maybe NT.NominalTag, GN.GlobalName)

type TopNameMap =
  Map.HashMap DD.DefiniteDescription TopNameInfo

data LookupResult a
  = Found a
  | Hidden
  | Missing
  deriving (Functor)

lookupAvailable :: SGL.StrictGlobalLocator -> [BN.BaseName] -> DD.DefiniteDescription -> TopNameMap -> LookupResult TopNameInfo
lookupAvailable currentLocator currentBodyContext dd topNameMap = do
  case Map.lookup dd topNameMap of
    Nothing ->
      Missing
    Just info
      | AV.allows currentLocator currentBodyContext dd ->
          Found info
      | otherwise ->
          Hidden
