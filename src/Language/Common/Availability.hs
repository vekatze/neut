module Language.Common.Availability
  ( allows,
    isRestricted,
  )
where

import Data.Text qualified as T
import Language.Common.BaseName qualified as BN
import Language.Common.Const (nsSep)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.SourcePrefix qualified as SP
import Language.Common.StrictGlobalLocator qualified as SGL

allows ::
  SGL.StrictGlobalLocator ->
  DD.DefiniteDescription ->
  Bool
allows currentLocator dd = do
  let (defModuleID, rest) = DD.unconsDD dd
  case SP.internalOwnerOf $ map BN.fromText $ T.splitOn nsSep rest of
    Nothing ->
      True
    Just owner ->
      defModuleID == SGL.moduleID currentLocator && owner `SP.contains` SGL.sourceLocator currentLocator

isRestricted :: DD.DefiniteDescription -> Bool
isRestricted dd = do
  let (_, rest) = DD.unconsDD dd
  case SP.internalOwnerOf $ map BN.fromText $ T.splitOn nsSep rest of
    Nothing ->
      False
    Just _ ->
      True
