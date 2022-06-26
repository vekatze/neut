{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Entity.Opacity where

import Data.Binary
import GHC.Generics

data Opacity
  = OpacityOpaque
  | OpacityTransparent
  deriving (Show, Eq, Generic)

instance Binary Opacity

isOpaque :: Opacity -> Bool
isOpaque o =
  case o of
    OpacityOpaque ->
      True
    _ ->
      False
