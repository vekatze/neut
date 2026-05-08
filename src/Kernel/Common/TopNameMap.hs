module Kernel.Common.TopNameMap
  ( TopNameInfo,
    TopNameMap,
    lookupAvailable,
  )
where

import Data.HashMap.Strict qualified as Map
import Kernel.Common.GlobalName qualified as GN
import Language.Common.Availability qualified as AV
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.NominalTag qualified as NT
import Language.Common.StrictGlobalLocator qualified as SGL
import Logger.Hint

type TopNameInfo =
  (Hint, Maybe NT.NominalTag, GN.GlobalName)

type TopNameMap =
  Map.HashMap DD.DefiniteDescription TopNameInfo

lookupAvailable :: SGL.StrictGlobalLocator -> DD.DefiniteDescription -> TopNameMap -> Maybe TopNameInfo
lookupAvailable currentLocator dd topNameMap = do
  info <- Map.lookup dd topNameMap
  if AV.allows currentLocator dd
    then return info
    else Nothing
