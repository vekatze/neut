module Language.Common.Rule.UnusedGlobalLocators (UnusedGlobalLocators, isUsedGL) where

import Aux.Logger.Rule.Hint
import Data.Text qualified as T

type UnusedGlobalLocators =
  [(T.Text, [(Hint, T.Text)])]

isUsedGL :: UnusedGlobalLocators -> T.Text -> Bool
isUsedGL unusedGlobalLocators cand =
  case unusedGlobalLocators of
    [] ->
      True
    (_, gls) : rest -> do
      notElem cand (map snd gls) && isUsedGL rest cand
