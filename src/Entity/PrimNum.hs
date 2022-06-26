{-# LANGUAGE DeriveGeneric #-}

module Entity.PrimNum where

import Data.Binary
import Entity.PrimNumSize
import qualified GHC.Generics as G

data PrimNum
  = PrimNumInt IntSize
  | PrimNumFloat FloatSize
  deriving (Show, G.Generic, Eq, Ord)

instance Binary PrimNum
