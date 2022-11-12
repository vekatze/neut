module Entity.EnsType where

import Context.Throw
import qualified Data.Text as T
import Entity.Hint

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

raiseTypeError :: Context m => Hint -> EnsType -> EnsType -> m a
raiseTypeError m expectedType actualType =
  raiseError m $
    "the value here is expected to be of type `"
      <> showEnsType expectedType
      <> "`, but is: `"
      <> showEnsType actualType
      <> "`"
