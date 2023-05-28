module Entity.Module where

import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.Ens qualified as E
import Entity.ModuleAlias
import Entity.ModuleChecksum
import Entity.ModuleID qualified as MID
import Entity.ModuleURL
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Target qualified as Target
import Path
import System.FilePath qualified as FP

type SomePath =
  Either (Path Abs Dir) (Path Abs File)

data Module = Module
  { moduleID :: MID.ModuleID,
    moduleTarget :: Map.HashMap Target.Target SGL.StrictGlobalLocator,
    moduleDependency :: Map.HashMap ModuleAlias (ModuleURL, ModuleChecksum),
    moduleExtraContents :: [SomePath],
    moduleAntecedents :: [ModuleChecksum],
    moduleLocation :: Path Abs File
  }
  deriving (Show)

getSourceDir :: Module -> Path Abs Dir
getSourceDir baseModule =
  getModuleRootDir baseModule </> sourceRelDir

getReleaseDir :: Module -> Path Abs Dir
getReleaseDir baseModule =
  getModuleRootDir baseModule </> releaseRelDir

getModuleRootDir :: Module -> Path Abs Dir
getModuleRootDir baseModule =
  parent $ moduleLocation baseModule

addDependency :: ModuleAlias -> ModuleURL -> ModuleChecksum -> Module -> Module
addDependency alias url checksum someModule =
  someModule {moduleDependency = Map.insert alias (url, checksum) (moduleDependency someModule)}

ppModule :: Module -> T.Text
ppModule someModule = do
  let entryPoint = Map.map (\x -> () :< E.String (SGL.getRelPathText x)) $ moduleTarget someModule
  let dependency = flip Map.map (moduleDependency someModule) $ \(ModuleURL url, ModuleChecksum checksum) -> do
        let urlEns = () :< E.String url
        let checksumEns = () :< E.String checksum
        () :< E.Dictionary (Map.fromList [("checksum", checksumEns), ("URL", urlEns)])
  let extraContents = map (\x -> () :< E.String (ppExtraContent x)) $ moduleExtraContents someModule
  let antecedents = map (\x -> () :< E.String (ppAntecedent x)) $ moduleAntecedents someModule
  E.ppEnsTopLevel $
    Map.fromList
      [ ("dependency", () :< E.Dictionary (Map.mapKeys (\(ModuleAlias key) -> BN.reify key) dependency)),
        ("target", () :< E.Dictionary (Map.mapKeys (\(Target.Target key) -> key) entryPoint)),
        ("extra-content", () :< E.List extraContents),
        ("antecedents", () :< E.List antecedents)
      ]

ppAntecedent :: ModuleChecksum -> T.Text
ppAntecedent (ModuleChecksum checksum) =
  checksum

ppExtraContent :: SomePath -> T.Text
ppExtraContent somePath =
  case somePath of
    Left dirPath ->
      T.pack $ toFilePath dirPath
    Right filePath ->
      T.pack $ toFilePath filePath

getID :: Module -> Module -> MID.ModuleID
getID mainModule currentModule = do
  if moduleLocation mainModule == moduleLocation currentModule
    then MID.Main
    else getChecksumFromModulePath (moduleLocation currentModule)

getChecksumFromModulePath :: Path Abs File -> MID.ModuleID
getChecksumFromModulePath moduleFilePath =
  MID.Library $
    ModuleChecksum $
      T.pack $
        FP.dropTrailingPathSeparator $
          toFilePath $
            dirname $
              parent moduleFilePath

getTargetList :: Module -> Maybe Target.Target -> [Target.Target]
getTargetList someModule mTarget =
  case mTarget of
    Just target ->
      [target]
    Nothing -> do
      Map.keys $ moduleTarget someModule
