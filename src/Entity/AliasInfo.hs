{-# LANGUAGE FlexibleInstances #-}

module Entity.AliasInfo where

import qualified Data.Text as T
import Entity.Hint

data AliasInfo
  = AliasInfoUse T.Text
  | AliasInfoPrefix Hint T.Text T.Text
  deriving (Show)
