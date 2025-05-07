module Kernel.Common.Rule.LocationTree
  ( LocationTree,
    LocType (..),
    SymbolName (..),
    empty,
    insert,
    find,
    findRef,
  )
where

import Data.Binary
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.IsConstLike
import Logger.Rule.Hint

type ColFrom =
  Int

type ColTo =
  Int

type DefSymbolLen =
  Int

type ColInterval =
  (ColFrom, ColTo)

data SymbolName
  = Local Int DefSymbolLen
  | Global DD.DefiniteDescription IsConstLike
  | Foreign EN.ExternalName
  deriving (Show, Eq, Generic)

data LocType
  = FileLoc
  | SymbolLoc SymbolName
  deriving (Show, Eq, Generic)

instance Binary SymbolName

instance Binary LocType

type LocationTree =
  M.Map (Line, ColFrom) (ColInterval, LocType, SavedHint)

empty :: LocationTree
empty =
  M.empty

insert :: LocType -> (Line, ColInterval) -> Hint -> LocationTree -> LocationTree
insert lt (l, (cFrom, cTo)) m =
  M.insert (l, cFrom) ((cFrom, cTo), lt, SavedHint m)

find :: Line -> Column -> LocationTree -> Maybe (LocType, Hint, ColInterval, DefSymbolLen)
find l c mp = do
  (colInterval@(colFrom, colTo), lt, SavedHint m) <- snd <$> M.lookupLE (l, c) mp
  case lt of
    FileLoc ->
      return (lt, m, colInterval, colTo - colFrom)
    SymbolLoc sym ->
      return (lt, m, colInterval, getLength sym)

getLength :: SymbolName -> DefSymbolLen
getLength s =
  case s of
    Local _ len ->
      len
    Global dd _ ->
      T.length $ DD.localLocator dd
    Foreign externalName ->
      T.length $ EN.reify externalName

isSymLoc :: LocType -> Bool
isSymLoc lt =
  case lt of
    SymbolLoc _ ->
      True
    FileLoc ->
      False

findRef :: Loc -> LocationTree -> [(FilePath, (Line, ColInterval))]
findRef loc t = do
  let kvs = M.toList t
  flip mapMaybe kvs $ \((line, _), (colInterval, lt, SavedHint m)) -> do
    if isSymLoc lt && loc == metaLocation m
      then return (metaFileName m, (line, colInterval))
      else Nothing
