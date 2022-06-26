{-# LANGUAGE DeriveGeneric #-}

module Entity.Ident where

import Data.Binary
import qualified Data.Text as T
import GHC.Generics

newtype Ident
  = I (T.Text, Int)
  deriving (Eq, Ord, Generic)

instance Show Ident where
  show (I (s, i)) =
    T.unpack s ++ "-" ++ show i

instance Binary Ident
