module Entity.TopNameMap
  ( TopNameMap,
    Visibility (..),
    isPublic,
  )
where

import Data.HashMap.Strict qualified as Map
import Entity.DefiniteDescription qualified as DD
import Entity.GlobalName qualified as GN
import Entity.Hint

type TopNameMap =
  Map.HashMap DD.DefiniteDescription (Visibility, (Hint, GN.GlobalName))

data Visibility
  = Public
  | Private
  deriving (Show, Eq)

isPublic :: Visibility -> Bool
isPublic v =
  case v of
    Public ->
      True
    Private ->
      False
