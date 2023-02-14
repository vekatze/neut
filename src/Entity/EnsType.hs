module Entity.EnsType where

import Data.Text qualified as T

data EnsType
  = Int64
  | Float64
  | Bool
  | String
  | List
  | Dictionary

showEnsType :: EnsType -> T.Text
showEnsType entityType =
  case entityType of
    Int64 ->
      "i64"
    Float64 ->
      "f64"
    Bool ->
      "bool"
    String ->
      "string"
    List ->
      "list"
    Dictionary ->
      "dictionary"
