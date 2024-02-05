module Entity.UnusedLocalLocators (UnusedLocalLocators, isUsedLL) where

import Entity.Hint
import Entity.LocalLocator qualified as LL

type UnusedLocalLocators =
  [(LL.LocalLocator, Hint)]

isUsedLL :: UnusedLocalLocators -> LL.LocalLocator -> Bool
isUsedLL unusedLocalLocators cand =
  case unusedLocalLocators of
    [] ->
      True
    (unusedLL, _) : rest ->
      (unusedLL /= cand) && isUsedLL rest cand
