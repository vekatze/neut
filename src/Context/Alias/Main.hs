module Context.Alias.Main (new) where

import qualified Context.Alias as Alias
import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Entity.Global
import Entity.Hint hiding (new)
import Entity.Module
import Entity.Source

new :: Alias.Config -> IO Alias.Context
new cfg = do
  locatorAliasMapRef <- newIORef Map.empty
  let moduleAliasMap = createModuleAliasMap (Alias.currentModule cfg) (Alias.mainModule cfg)
  return
    Alias.Context
      { Alias.getCandList =
          getCandList (Alias.locatorCtx cfg) moduleAliasMap locatorAliasMapRef,
        Alias.registerLocatorAlias =
          registerLocatorAlias (Alias.throwCtx cfg) locatorAliasMapRef
      }

createModuleAliasMap :: Module -> Module -> Map.HashMap T.Text T.Text
createModuleAliasMap currentModule mainModule = do
  let additionalChecksumAlias = getAdditionalChecksumAlias currentModule mainModule
  Map.fromList $ Maybe.catMaybes [additionalChecksumAlias] ++ getModuleChecksumAliasList currentModule

getAdditionalChecksumAlias :: Module -> Module -> Maybe (T.Text, T.Text)
getAdditionalChecksumAlias currentModule mainModule = do
  let domain = getDomain currentModule mainModule
  if defaultModulePrefix == domain
    then Nothing
    else return (defaultModulePrefix, domain)

getCandList ::
  Locator.Context ->
  Map.HashMap T.Text T.Text ->
  IORef (Map.HashMap T.Text T.Text) ->
  T.Text ->
  Bool ->
  IO [T.Text]
getCandList ctx moduleAliasMap locatorAliasMapRef name isDefinite = do
  nameListWithAlias <- Locator.getPossibleReferents ctx name isDefinite
  locatorAliasMap <- readIORef locatorAliasMapRef
  return $ map (resolveAlias nsSep moduleAliasMap . resolveAlias definiteSep locatorAliasMap) nameListWithAlias

resolveAlias :: T.Text -> Map.HashMap T.Text T.Text -> T.Text -> T.Text
resolveAlias sep aliasMap currentName = do
  case T.breakOn sep currentName of
    (prefix, suffix)
      | not (T.null suffix),
        Just newPrefix <- Map.lookup prefix aliasMap ->
        resolveAlias sep aliasMap $ newPrefix <> suffix
      | otherwise ->
        currentName

registerLocatorAlias :: Throw.Context -> IORef (Map.HashMap T.Text T.Text) -> Hint -> T.Text -> T.Text -> IO ()
registerLocatorAlias ctx locatorAliasMapRef m from to = do
  aliasEnv <- readIORef locatorAliasMapRef
  if Map.member from aliasEnv
    then Throw.raiseError ctx m $ "the prefix `" <> from <> "` is already registered"
    else writeIORef locatorAliasMapRef $ Map.insert from to aliasEnv
