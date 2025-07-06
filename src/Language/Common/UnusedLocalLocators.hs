module Language.Common.UnusedLocalLocators (UnusedLocalLocators, isUsedLL) where

import Language.Common.LocalLocator qualified as LL
import Logger.Hint

type UnusedLocalLocators =
  [(LL.LocalLocator, Hint)]

isUsedLL :: UnusedLocalLocators -> LL.LocalLocator -> Bool
isUsedLL unusedLocalLocators cand =
  case unusedLocalLocators of
    [] ->
      True
    (unusedLL, _) : rest ->
      (unusedLL /= cand) && isUsedLL rest cand
