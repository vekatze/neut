module Rule.UnusedLocalLocators (UnusedLocalLocators, isUsedLL) where

import Rule.Hint
import Rule.LocalLocator qualified as LL

type UnusedLocalLocators =
  [(LL.LocalLocator, Hint)]

isUsedLL :: UnusedLocalLocators -> LL.LocalLocator -> Bool
isUsedLL unusedLocalLocators cand =
  case unusedLocalLocators of
    [] ->
      True
    (unusedLL, _) : rest ->
      (unusedLL /= cand) && isUsedLL rest cand
