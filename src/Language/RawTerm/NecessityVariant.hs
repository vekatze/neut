module Language.RawTerm.NecessityVariant
  ( NecessityVariant (..),
    showNecessityVariant,
    layerOffset,
    fromText,
  )
where

import Data.Text qualified as T

data NecessityVariant
  = VariantK
  | VariantT

showNecessityVariant :: NecessityVariant -> T.Text
showNecessityVariant nv =
  case nv of
    VariantK ->
      "letbox"
    VariantT ->
      "letbox-T"

fromText :: T.Text -> Maybe NecessityVariant
fromText t =
  case t of
    "letbox" ->
      return VariantK
    "letbox-T" ->
      return VariantT
    _ ->
      Nothing

layerOffset :: NecessityVariant -> Int
layerOffset nv =
  case nv of
    VariantK ->
      1
    VariantT ->
      0
