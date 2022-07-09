module Context.Alias.Main (new) where

import qualified Context.Alias as Alias
import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Entity.Const
import Entity.Hint hiding (new)
import Entity.Module
import Entity.ModuleAlias
import Entity.ModuleChecksum
import qualified Entity.ModuleID as MID

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

createModuleAliasMap :: Module -> Module -> Map.HashMap ModuleAlias ModuleChecksum
createModuleAliasMap currentModule mainModule = do
  let additionalChecksumAlias = getAliasForThis mainModule currentModule
  Map.fromList $ Maybe.catMaybes [additionalChecksumAlias] ++ getModuleChecksumAliasList currentModule

getAliasForThis :: Module -> Module -> Maybe (ModuleAlias, ModuleChecksum)
getAliasForThis mainModule currentModule = do
  case MID.getModuleID mainModule currentModule of
    MID.This ->
      Nothing
    MID.That checksum ->
      return (ModuleAlias defaultModulePrefix, checksum)

getCandList ::
  Locator.Context ->
  Map.HashMap ModuleAlias ModuleChecksum ->
  IORef (Map.HashMap T.Text T.Text) ->
  T.Text ->
  Bool ->
  IO [T.Text]
getCandList ctx moduleAliasMap locatorAliasMapRef name isDefinite = do
  nameListWithAlias <- Locator.getPossibleReferents ctx name isDefinite
  locatorAliasMap <- readIORef locatorAliasMapRef
  return $ map (resolveModuleAlias moduleAliasMap . resolveAlias definiteSep locatorAliasMap) nameListWithAlias

resolveAlias :: T.Text -> Map.HashMap T.Text T.Text -> T.Text -> T.Text
resolveAlias sep aliasMap currentName = do
  case T.breakOn sep currentName of
    (prefix, suffix)
      | not (T.null suffix),
        Just newPrefix <- Map.lookup prefix aliasMap ->
        resolveAlias sep aliasMap $ newPrefix <> suffix
      | otherwise ->
        currentName

resolveModuleAlias :: Map.HashMap ModuleAlias ModuleChecksum -> T.Text -> T.Text
resolveModuleAlias aliasMap currentName = do
  case T.breakOn nsSep currentName of
    (prefix, suffix) -- ("foo", ".bar.buz::qux.some-func")
      | not (T.null suffix),
        Just (ModuleChecksum checksum) <- Map.lookup (ModuleAlias prefix) aliasMap ->
        checksum <> suffix
      | otherwise ->
        currentName

registerLocatorAlias :: Throw.Context -> IORef (Map.HashMap T.Text T.Text) -> Hint -> T.Text -> T.Text -> IO ()
registerLocatorAlias ctx locatorAliasMapRef m from to = do
  aliasEnv <- readIORef locatorAliasMapRef
  if Map.member from aliasEnv
    then Throw.raiseError ctx m $ "the prefix `" <> from <> "` is already registered"
    else writeIORef locatorAliasMapRef $ Map.insert from to aliasEnv
