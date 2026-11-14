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

fromText :: T.Text -> Either Error OutputKind
fromText t =
  case t of
    "object" ->
      return Object
    "llvm" ->
      return LLVM
    _ ->
      Left $ newError' $ "Invalid output kind: " <> t
