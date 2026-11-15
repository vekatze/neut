module Command.Common.ExposedName (findConflicts) where

import App.Error qualified as E
import Data.HashMap.Strict qualified as Map
import Data.List qualified as List
import Kernel.Common.Cache qualified as Cache
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Parse.Parse (SourceUnit)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Term.Stmt (StmtF (StmtExpose))
import Language.WeakTerm.WeakStmt (WeakStmt (WeakStmtExpose))
import Logger.Hint

type ExposedName =
  (Hint, DD.DefiniteDescription, EN.ExternalName)

findConflicts :: ModulePath.ModulePathMap -> [SourceUnit (Either Cache.Cache ([WeakStmt], a), b)] -> [E.Error]
findConflicts modulePathMap =
  findConflicts' modulePathMap Map.empty . List.sortOn getSourcePosition . concatMap getExposedNames

getSourcePosition :: ExposedName -> (FilePath, Loc)
getSourcePosition (m, _, _) =
  (metaFileName m, metaLocation m)

getExposedNames :: SourceUnit (Either Cache.Cache ([WeakStmt], a), b) -> [ExposedName]
getExposedNames (_, _, (_, (cacheOrStmtList, _))) =
  case cacheOrStmtList of
    Left cache ->
      [(m, dd, extName) | StmtExpose exportList <- Cache.stmtList cache, (SavedHint m, dd, extName, _) <- exportList]
    Right (stmtList, _) ->
      [(m, dd, extName) | WeakStmtExpose exportList <- stmtList, (m, dd, extName) <- exportList]

findConflicts' :: ModulePath.ModulePathMap -> Map.HashMap EN.ExternalName DD.DefiniteDescription -> [ExposedName] -> [E.Error]
findConflicts' modulePathMap found exposedNameList =
  case exposedNameList of
    [] ->
      []
    (m, dd, extName) : rest ->
      case Map.lookup extName found of
        Just ddExposed ->
          makeConflictError modulePathMap m ddExposed extName : findConflicts' modulePathMap found rest
        Nothing ->
          findConflicts' modulePathMap (Map.insert extName dd found) rest

makeConflictError :: ModulePath.ModulePathMap -> Hint -> DD.DefiniteDescription -> EN.ExternalName -> E.Error
makeConflictError modulePathMap m ddExposed extName =
  E.newError m $
    "`"
      <> EN.reify extName
      <> "` is already exposed by `"
      <> ModulePath.renderCanonicalDD modulePathMap ddExposed
      <> "`"
