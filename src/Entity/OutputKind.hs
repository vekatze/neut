module Entity.OutputKind where

data OutputKind
  = Object
  | LLVM
  | Asm
  deriving (Show, Eq)

instance Read OutputKind where
  readsPrec _ "object" =
    [(Object, [])]
  readsPrec _ "llvm" =
    [(LLVM, [])]
  readsPrec _ "asm" =
    [(Asm, [])]
  readsPrec _ _ =
    []
