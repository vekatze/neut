module Kernel.Common.Rule.OutputKind
  ( OutputKind (..),
    fromText,
  )
where

import Data.Text qualified as T
import Language.Common.Rule.Error (newError')
import Library.Error.Rule.Error (Error)

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

fromText :: T.Text -> Either Error OutputKind
fromText t =
  case t of
    "object" ->
      return Object
    "llvm" ->
      return LLVM
    _ ->
      Left $ newError' $ "Invalid output kind: " <> t
