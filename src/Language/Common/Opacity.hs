module Language.Common.Opacity
  ( Opacity (..),
    isOpaque,
  )
where

import Binary.Derive (deriveBinary)

data Opacity
  = Opaque
  | Clear
  deriving (Show, Eq)

isOpaque :: Opacity -> Bool
isOpaque o =
  case o of
    Opaque ->
      True
    _ ->
      False

deriveBinary [''Opacity]
