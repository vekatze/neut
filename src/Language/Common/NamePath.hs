module Language.Common.NamePath
  ( normalize,
    normalizeText,
    reify,
  )
where

import Data.Text qualified as T
import Language.Common.BaseName qualified as BN
import Language.Common.Const (nsSep)

normalize :: [BN.BaseName] -> [BN.BaseName]
normalize =
  filter (/= BN.this)

normalizeText :: [T.Text] -> [T.Text]
normalizeText =
  filter (/= BN.reify BN.this)

reify :: [BN.BaseName] -> T.Text
reify segments = do
  case segments of
    [] ->
      BN.reify BN.this
    _ ->
      T.intercalate nsSep $ map BN.reify segments
