module Entity.Module where

import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.Ens qualified as E
import Entity.ModuleAlias
import Entity.ModuleDigest
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
    moduleDependency :: Map.HashMap ModuleAlias (ModuleURL, ModuleDigest),
    moduleExtraContents :: [SomePath],
    moduleAntecedents :: [ModuleDigest],
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

addDependency :: ModuleAlias -> ModuleURL -> ModuleDigest -> Module -> Module
addDependency alias url digest someModule =
  someModule {moduleDependency = Map.insert alias (url, digest) (moduleDependency someModule)}

getDigestMap :: Module -> Map.HashMap ModuleDigest (NE.NonEmpty ModuleAlias)
getDigestMap baseModule = do
  let dep = moduleDependency baseModule
  let foo = map (\(alias, (_, digest)) -> (digest, alias)) $ Map.toList dep
  let groupedFoo = NE.groupBy (\(c1, _) (c2, _) -> c1 == c2) foo
  Map.fromList $ flip map groupedFoo $ \item -> do
    let representative = fst $ NE.head item
    let elems = NE.map snd item
    (representative, elems)

ppModule :: Module -> T.Text
ppModule someModule = do
  let entryPoint = Map.map (\x -> () :< E.String (SGL.getRelPathText x)) $ moduleTarget someModule
  let dependency = flip Map.map (moduleDependency someModule) $ \(ModuleURL url, ModuleDigest digest) -> do
        let urlEns = () :< E.String url
        let digestEns = () :< E.String digest
        () :< E.Dictionary (Map.fromList [("digest", digestEns), ("URL", urlEns)])
  let extraContents = map (\x -> () :< E.String (ppExtraContent x)) $ moduleExtraContents someModule
  let antecedents = map (\x -> () :< E.String (ppAntecedent x)) $ moduleAntecedents someModule
  E.ppEnsTopLevel $
    Map.fromList
      [ ("dependency", () :< E.Dictionary (Map.mapKeys (\(ModuleAlias key) -> BN.reify key) dependency)),
        ("target", () :< E.Dictionary (Map.mapKeys (\(Target.Target key) -> key) entryPoint)),
        ("extra-content", () :< E.List extraContents),
        ("antecedent", () :< E.List antecedents)
      ]

ppAntecedent :: ModuleDigest -> T.Text
ppAntecedent (ModuleDigest digest) =
  digest

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
    else getDigestFromModulePath (moduleLocation currentModule)

getDigestFromModulePath :: Path Abs File -> MID.ModuleID
getDigestFromModulePath moduleFilePath =
  MID.Library $
    ModuleDigest $
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
