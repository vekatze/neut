module Language.Common.Rule.UnusedLocalLocators (UnusedLocalLocators, isUsedLL) where

import Aux.Logger.Rule.Hint
import Language.Common.Rule.LocalLocator qualified as LL

type UnusedLocalLocators =
  [(LL.LocalLocator, Hint)]

isUsedLL :: UnusedLocalLocators -> LL.LocalLocator -> Bool
isUsedLL unusedLocalLocators cand =
  case unusedLocalLocators of
    [] ->
      True
    (unusedLL, _) : rest ->
      (unusedLL /= cand) && isUsedLL rest cand
