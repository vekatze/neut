module Kernel.Common.LocationTree
  ( LocationTree,
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
  = Local Int DefSymbolLen
  | Global DD.DefiniteDescription IsConstLike
  | Foreign EN.ExternalName
  | StaticFile T.Text
  | SourceFile T.Text
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

find :: Line -> Column -> LocationTree -> Maybe (SymbolName, Hint, ColInterval, DefSymbolLen)
find l c mp = do
  (line, colInterval@(_, colTo), sym, SavedHint m) <- snd <$> M.lookupLE (l, c) mp
  if colTo < c || line /= l
    then Nothing
    else return (sym, m, colInterval, getLength sym)

getLength :: SymbolName -> DefSymbolLen
getLength s =
  case s of
    Local _ len ->
      len
    Global dd _ ->
      T.length $ DD.localLocator dd
    Foreign externalName ->
      T.length $ EN.reify externalName
    StaticFile key ->
      T.length key
    SourceFile locator ->
      T.length locator

findRef :: Loc -> LocationTree -> [(FilePath, (Line, ColInterval))]
findRef loc t = do
  let kvs = M.toList t
  flip mapMaybe kvs $ \((line, _), (_, colInterval, _, SavedHint m)) -> do
    if loc == metaLocation m
      then return (metaFileName m, (line, colInterval))
      else Nothing
