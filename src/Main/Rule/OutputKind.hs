module Main.Rule.OutputKind (OutputKind (..)) where

data OutputKind
  = Object
  | LLVM
  deriving (Show, Eq)

instance Read OutputKind where
  readsPrec _ "object" =
    [(Object, [])]
  readsPrec _ "llvm" =
    [(LLVM, [])]
  readsPrec _ _ =
    []
