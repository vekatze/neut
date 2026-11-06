module Language.Comp.EnumCase (EnumCase (..)) where


newtype EnumCase
  = Int Integer
  deriving (Show, Eq, Ord)
