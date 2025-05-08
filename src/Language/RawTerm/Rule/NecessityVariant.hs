module Language.RawTerm.Rule.NecessityVariant
  ( NecessityVariant (..),
    showNecessityVariant,
    layerOffset,
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

layerOffset :: NecessityVariant -> Int
layerOffset nv =
  case nv of
    VariantK ->
      1
    VariantT ->
      0
