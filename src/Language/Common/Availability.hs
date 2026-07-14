module Language.Common.Availability
  ( allows,
    allowsAsSignatureDependency,
    isRestricted,
  )
where

import Data.Maybe
import Data.Text qualified as T
import Language.Common.BaseName qualified as BN
import Language.Common.Const
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.StrictGlobalLocator qualified as SGL

allows ::
  SGL.StrictGlobalLocator ->
  [BN.BaseName] ->
  DD.DefiniteDescription ->
  Bool
allows currentLocator currentBodyContext dd = do
  case requiredPrefixOf dd of
    Nothing ->
      True
    Just requiredPrefix -> do
      let bodyPath = T.intercalate nsSep $ map BN.reify currentBodyContext
      let currentAddress = SGL.reify currentLocator <> doubleColon <> bodyPath
      requiredPrefix `isAddressPrefixOf` currentAddress

allowsAsSignatureDependency ::
  DD.DefiniteDescription ->
  DD.DefiniteDescription ->
  Bool
allowsAsSignatureDependency ownerDD depDD = do
  case (requiredPrefixOf depDD, requiredPrefixOf ownerDD) of
    (Nothing, _) ->
      True
    (Just _, Nothing) ->
      False
    (Just depPrefix, Just ownerPrefix) ->
      depPrefix `isAddressPrefixOf` ownerPrefix

isRestricted :: DD.DefiniteDescription -> Bool
isRestricted =
  isJust . requiredPrefixOf

requiredPrefixOf :: DD.DefiniteDescription -> Maybe T.Text
requiredPrefixOf dd = do
  case DD.globalParts dd of
    Nothing ->
      Nothing
    Just (modulePathText, sourcePathText, bodyPathText) -> do
      case internalPrefixOf bodyPathText of
        Just bodyPrefix ->
          Just $ appendNonEmpty (modulePathText <> doubleColon <> sourcePathText) doubleColon bodyPrefix
        Nothing -> do
          case internalPrefixOf sourcePathText of
            Just sourcePrefix ->
              Just $ appendNonEmpty modulePathText doubleColon sourcePrefix
            Nothing ->
              Nothing

internalPrefixOf :: T.Text -> Maybe T.Text
internalPrefixOf pathText = do
  let reversedSegments = reverse $ T.splitOn nsSep pathText
  case dropWhile (not . T.isPrefixOf "_") reversedSegments of
    [] ->
      Nothing
    _ : precedingSegments ->
      Just $ T.intercalate nsSep $ reverse precedingSegments

appendNonEmpty :: T.Text -> T.Text -> T.Text -> T.Text
appendNonEmpty prefix separator suffix = do
  if T.null suffix
    then prefix
    else prefix <> separator <> suffix

isAddressPrefixOf :: T.Text -> T.Text -> Bool
isAddressPrefixOf required current = do
  case T.stripPrefix required current of
    Nothing ->
      False
    Just rest ->
      T.null rest || nsSep `T.isPrefixOf` rest || doubleColon `T.isPrefixOf` rest
