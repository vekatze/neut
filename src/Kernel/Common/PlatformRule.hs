module Kernel.Common.PlatformRule
  ( PlatformRule (..),
    defaultPlatform,
    selectRule,
  )
where

import Data.List (find)
import Kernel.Common.PlatformPattern (PlatformPattern, sameHostAndTarget)
import Logger.Hint (Hint, internalHint)

data PlatformRule a = PlatformRule
  { rulePlatform :: (Hint, PlatformPattern),
    ruleBody :: a
  }
  deriving (Show, Eq)

defaultPlatform :: (Hint, PlatformPattern)
defaultPlatform =
  (internalHint, sameHostAndTarget)

selectRule :: (PlatformPattern -> Bool) -> [PlatformRule a] -> Maybe (PlatformRule a)
selectRule predicate =
  find (predicate . snd . rulePlatform)
