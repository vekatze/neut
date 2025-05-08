module Kernel.Common.Rule.UnusedLocalLocators (UnusedLocalLocators, isUsedLL) where

import Language.Common.Rule.LocalLocator qualified as LL
import Library.Logger.Rule.Hint

type UnusedLocalLocators =
  [(LL.LocalLocator, Hint)]

isUsedLL :: UnusedLocalLocators -> LL.LocalLocator -> Bool
isUsedLL unusedLocalLocators cand =
  case unusedLocalLocators of
    [] ->
      True
    (unusedLL, _) : rest ->
      (unusedLL /= cand) && isUsedLL rest cand
