module Kernel.Common.LocationTree
  ( LocationTree,
    SymbolName (..),
    empty,
    insert,
    retarget,
    find,
    findRef,
    findSymbolRef,
  )
where

import Data.Binary
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.IsConstLike
import Logger.Hint

type ColFrom =
  Int

type ColTo =
  Int

type DefSymbolLen =
  Int

type ColInterval =
  (ColFrom, ColTo)

data SymbolName
  = Local Int DefSymbolLen FilePath Loc
  | Global DD.DefiniteDescription IsConstLike
  | Foreign EN.ExternalName
  | NamespaceView FilePath Loc
  | StaticFile T.Text
  | SourceFile T.Text
  | ModuleFile FilePath
  deriving (Show, Eq, Generic)

instance Binary SymbolName

type LocationTree =
  M.Map (Line, ColFrom) (Line, ColInterval, SymbolName, SavedHint)

empty :: LocationTree
empty =
  M.empty

insert :: SymbolName -> (Line, ColInterval) -> Hint -> LocationTree -> LocationTree
insert sym (l, (cFrom, cTo)) m =
  M.insert (l, cFrom) (l, (cFrom, cTo), sym, SavedHint m)

retarget :: (Line, ColFrom) -> Hint -> LocationTree -> LocationTree
retarget key m =
  M.adjust (\(line, interval, sym, _) -> (line, interval, sym, SavedHint m)) key

find :: Line -> Column -> LocationTree -> Maybe (SymbolName, Hint, ColInterval, DefSymbolLen)
find l c mp = do
  (line, colInterval@(_, colTo), sym, SavedHint m) <- snd <$> M.lookupLE (l, c) mp
  if colTo < c || line /= l
    then Nothing
    else return (sym, m, colInterval, getDefinitionLength sym)

getDefinitionLength :: SymbolName -> DefSymbolLen
getDefinitionLength s =
  case s of
    Local _ len _ _ ->
      len
    Global dd _ ->
      T.length $ DD.baseNameText dd
    Foreign externalName ->
      T.length $ EN.reify externalName
    _ ->
      0

findRef :: Loc -> LocationTree -> [(FilePath, (Line, ColInterval))]
findRef loc t = do
  let kvs = M.toList t
  flip mapMaybe kvs $ \((line, _), (_, colInterval, _, SavedHint m)) -> do
    if loc == metaLocation m
      then return (metaFileName m, (line, colInterval))
      else Nothing

findSymbolRef :: SymbolName -> LocationTree -> [(FilePath, (Line, ColInterval))]
findSymbolRef symbolName t = do
  let kvs = M.toList t
  flip mapMaybe kvs $ \((line, _), (_, colInterval, symbolName', SavedHint m)) -> do
    if symbolName == symbolName'
      then return (metaFileName m, (line, colInterval))
      else Nothing
