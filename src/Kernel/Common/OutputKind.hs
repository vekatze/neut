module Kernel.Common.OutputKind
  ( OutputKind (..),
    fromText,
  )
where

import App.Error (Error, newError')
import Data.Text qualified as T

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
