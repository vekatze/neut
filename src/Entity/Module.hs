module Entity.Module where

import Control.Comonad.Cofree
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.ModuleAlias
import Entity.ModuleDigest
import Entity.ModuleID qualified as MID
import Entity.ModuleURL
import Entity.StrictGlobalLocator (StrictGlobalLocator)
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Target qualified as Target
import Entity.Tree qualified as TR
import Path
import System.FilePath qualified as FP

type SomePath =
  Either (Path Abs Dir) (Path Abs File)

data Module = Module
  { moduleID :: MID.ModuleID,
    moduleTarget :: Map.HashMap Target.Target SGL.StrictGlobalLocator,
    moduleDependency :: Map.HashMap ModuleAlias ([ModuleURL], ModuleDigest),
    moduleExtraContents :: [SomePath],
    moduleAntecedents :: [ModuleDigest],
    moduleLocation :: Path Abs File,
    moduleForeignDirList :: [Path Abs Dir]
  }
  deriving (Show)

keyTarget :: T.Text
keyTarget =
  "target"

keyDependency :: T.Text
keyDependency =
  "dependency"

keyExtraContent :: T.Text
keyExtraContent =
  "extra-content"

keyAntecedent :: T.Text
keyAntecedent =
  "antecedent"

keyForeign :: T.Text
keyForeign =
  "foreign"

getSourceDir :: Module -> Path Abs Dir
getSourceDir baseModule =
  getModuleRootDir baseModule </> sourceRelDir

getReleaseDir :: Module -> Path Abs Dir
getReleaseDir baseModule =
  getModuleRootDir baseModule </> releaseRelDir

getModuleRootDir :: Module -> Path Abs Dir
getModuleRootDir baseModule =
  parent $ moduleLocation baseModule

addDependency :: ModuleAlias -> [ModuleURL] -> ModuleDigest -> Module -> Module
addDependency alias mirrorList digest someModule = do
  someModule {moduleDependency = Map.insert alias (mirrorList, digest) (moduleDependency someModule)}

insertDependency :: ([ModuleURL], [ModuleDigest]) -> ([ModuleURL], [ModuleDigest]) -> ([ModuleURL], [ModuleDigest])
insertDependency (mirrorList1, digest1) (mirrorList2, _) = do
  (nubOrd $ mirrorList1 ++ mirrorList2, digest1)

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
  TR.ppTreeList
    ( (),
      catMaybes
        [ nodeOrNone $
            symbol keyTarget
              : map ppEntryPoint (Map.toList (moduleTarget someModule)),
          nodeOrNone $
            symbol keyExtraContent
              : map (string . ppExtraContent) (moduleExtraContents someModule),
          nodeOrNone $
            symbol keyForeign
              : map (string . ppForeignContent) (moduleForeignDirList someModule),
          nodeOrNone $
            symbol keyAntecedent
              : map (string . ppAntecedent) (moduleAntecedents someModule),
          nodeOrNone $
            symbol keyDependency
              : map ppDependency (Map.toList (moduleDependency someModule))
        ]
    )

type Tree = Cofree TR.TreeF ()

symbol :: T.Text -> Tree
symbol a =
  () :< TR.Symbol a

string :: T.Text -> Tree
string str =
  () :< TR.String str

node :: [Tree] -> Tree
node ts =
  () :< TR.Node ts

nodeOrNone :: [Tree] -> Maybe Tree
nodeOrNone ts =
  if length ts <= 1
    then Nothing
    else return $ () :< TR.Node ts

ppEntryPoint :: (Target.Target, StrictGlobalLocator) -> Tree
ppEntryPoint (Target.Target target, sgl) = do
  node [symbol target, string (SGL.getRelPathText sgl)]

ppDependency :: (ModuleAlias, ([ModuleURL], ModuleDigest)) -> Tree
ppDependency (ModuleAlias alias, (urlList, ModuleDigest digest)) = do
  node
    [ symbol (BN.reify alias),
      node [symbol "digest", string digest],
      node (symbol "mirror" : map (\(ModuleURL urlText) -> string urlText) urlList)
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

ppForeignContent :: Path Abs Dir -> T.Text
ppForeignContent dirPath =
  T.pack $ toFilePath dirPath

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
