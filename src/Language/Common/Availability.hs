module Language.Common.Availability
  ( allows,
    allowsAsSignatureDependency,
    isRestricted,
  )
where

import Data.Text qualified as T
import Language.Common.BaseName qualified as BN
import Language.Common.Const (nsSep)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ModuleID qualified as MID
import Language.Common.SourcePrefix qualified as SP
import Language.Common.StrictGlobalLocator qualified as SGL

allows ::
  SGL.StrictGlobalLocator ->
  DD.DefiniteDescription ->
  Bool
allows currentLocator dd = do
  case requiredPrefixOf dd of
    Nothing ->
      True
    Just (defModuleID, owner) ->
      defModuleID == SGL.moduleID currentLocator && owner `SP.contains` SGL.sourceLocator currentLocator

allowsAsSignatureDependency ::
  DD.DefiniteDescription ->
  DD.DefiniteDescription ->
  Bool
allowsAsSignatureDependency ownerDD depDD = do
  case requiredPrefixOf depDD of
    Nothing ->
      True
    Just (depModuleID, depPrefix) ->
      case requiredPrefixOf ownerDD of
        Nothing ->
          False
        Just (ownerModuleID, ownerPrefix) ->
          depModuleID == ownerModuleID && depPrefix `SP.isPrefixOf` ownerPrefix

isRestricted :: DD.DefiniteDescription -> Bool
isRestricted dd = do
  case requiredPrefixOf dd of
    Nothing ->
      False
    Just _ ->
      True

requiredPrefixOf :: DD.DefiniteDescription -> Maybe (MID.ModuleID, SP.SourcePrefix)
requiredPrefixOf dd = do
  let (moduleID, rest) = DD.unconsDD dd
  prefix <- SP.internalOwnerOf $ map BN.fromText $ T.splitOn nsSep rest
  return (moduleID, prefix)
