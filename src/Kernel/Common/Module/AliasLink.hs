module Kernel.Common.Module.AliasLink
  ( placeAliasLinks,
  )
where

import App.App (App)
import App.Run (raiseError')
import Control.Exception (IOException, try)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as HashSet
import Data.List (isPrefixOf)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Unique (hashUnique, newUnique)
import Kernel.Common.Module qualified as M
import Kernel.Common.Module.GetDependency qualified as GetDependency
import Language.Common.ModuleAlias qualified as MA
import Language.Common.ModuleID qualified as MID
import Logger.Hint qualified as H
import Path (toFilePath, (</>))
import System.Directory qualified as D
import System.FilePath qualified as FP
import System.Posix.Files qualified as PF
import System.Posix.Process (getProcessID)

data Occupant
  = Vacant
  | Link FilePath
  | NotLink

placeAliasLinks :: GetDependency.Handle -> M.MainModule -> App ()
placeAliasLinks h mainModule =
  placeAliasLinks' h HashSet.empty [M.extractModule mainModule]

placeAliasLinks' :: GetDependency.Handle -> HashSet.HashSet MID.ModuleID -> [M.Module] -> App ()
placeAliasLinks' h visited queue =
  case queue of
    [] ->
      return ()
    currentModule : rest
      | HashSet.member (M.moduleID currentModule) visited ->
          placeAliasLinks' h visited rest
      | otherwise -> do
          dependencyList <- getDependencyList h currentModule
          placeAliasLinksOf currentModule dependencyList
          let visited' = HashSet.insert (M.moduleID currentModule) visited
          placeAliasLinks' h visited' (rest <> map snd dependencyList)

getDependencyList :: GetDependency.Handle -> M.Module -> App [(MA.ModuleAlias, M.Module)]
getDependencyList h currentModule = do
  let m = H.newSourceHint $ M.moduleLocation currentModule
  forM (Map.toList $ M.moduleDependency currentModule) $ \(alias, dependency) -> do
    dependencyModule <- GetDependency.getDependency h m alias dependency
    return (alias, dependencyModule)

placeAliasLinksOf :: M.Module -> [(MA.ModuleAlias, M.Module)] -> App ()
placeAliasLinksOf currentModule dependencyList = do
  let cacheDir = M.getModuleRootDir currentModule </> M.moduleCacheDir currentModule
  let aliasDir = toFilePath cacheDir FP.</> "alias"
  aliasDirExists <- liftIO $ D.doesDirectoryExist aliasDir
  unless (null dependencyList && not aliasDirExists) $ do
    liftIO $ D.createDirectoryIfMissing True aliasDir
    realAliasDir <- liftIO $ D.canonicalizePath aliasDir
    placeAliasLinksIn realAliasDir dependencyList

placeAliasLinksIn :: FilePath -> [(MA.ModuleAlias, M.Module)] -> App ()
placeAliasLinksIn aliasDir dependencyList = do
  forM_ dependencyList $ \(alias, dependencyModule) -> do
    targetDir <- liftIO $ D.canonicalizePath $ toFilePath $ M.getModuleRootDir dependencyModule
    placeLink aliasDir (T.unpack $ MA.reify alias) (getRelativePath aliasDir targetDir)
  let aliasNameSet = S.fromList $ map (T.unpack . MA.reify . fst) dependencyList
  entryList <- liftIO $ D.listDirectory aliasDir
  forM_ entryList $ \entry -> do
    unless (S.member entry aliasNameSet || isTemporaryName entry) $ do
      occupant <- liftIO $ getOccupant $ aliasDir FP.</> entry
      case occupant of
        Link _ ->
          liftIO $ PF.removeLink $ aliasDir FP.</> entry
        _ ->
          return ()

placeLink :: FilePath -> FilePath -> FilePath -> App ()
placeLink aliasDir aliasName target = do
  let linkPath = aliasDir FP.</> aliasName
  occupant <- liftIO $ getOccupant linkPath
  case occupant of
    Link currentTarget
      | currentTarget == target ->
          return ()
    NotLink ->
      raiseError' $ "Could not place a link at " <> T.pack linkPath <> ", since something other than a link is there"
    _ -> liftIO $ do
      processID <- getProcessID
      unique <- newUnique
      let temporaryPath = aliasDir FP.</> ("." <> aliasName <> "." <> show processID <> "." <> show (hashUnique unique))
      PF.createSymbolicLink target temporaryPath
      PF.rename temporaryPath linkPath

getOccupant :: FilePath -> IO Occupant
getOccupant path = do
  statusOrError <- try $ PF.getSymbolicLinkStatus path
  case statusOrError of
    Left err ->
      const (return Vacant) (err :: IOException)
    Right status
      | PF.isSymbolicLink status ->
          Link <$> PF.readSymbolicLink path
      | otherwise ->
          return NotLink

isTemporaryName :: FilePath -> Bool
isTemporaryName =
  isPrefixOf "."

getRelativePath :: FilePath -> FilePath -> FilePath
getRelativePath fromDir toDir = do
  let fromParts = FP.splitDirectories fromDir
  let toParts = FP.splitDirectories toDir
  let sharedCount = length $ takeWhile id $ zipWith (==) fromParts toParts
  FP.joinPath $ replicate (length fromParts - sharedCount) ".." <> drop sharedCount toParts
