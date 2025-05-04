module Main.Rule.UnusedGlobalLocators (UnusedGlobalLocators, isUsedGL) where

import Data.Text qualified as T
import Language.Common.Rule.Hint

type UnusedGlobalLocators =
  [(T.Text, [(Hint, T.Text)])]

isUsedGL :: UnusedGlobalLocators -> T.Text -> Bool
isUsedGL unusedGlobalLocators cand =
  case unusedGlobalLocators of
    [] ->
      True
    (_, gls) : rest -> do
      notElem cand (map snd gls) && isUsedGL rest cand
