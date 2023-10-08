module Entity.Macro
  ( Arg,
    ArgF (..),
    Args,
    Rules,
    Sub,
    MacroInfo,
    PreMacroInfo,
    showArg,
    showArgs,
  )
where

import Control.Comonad.Cofree
import Data.Binary
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.DefiniteDescription qualified as DD
import Entity.Hint (Hint)
import Entity.RawIdent
import Entity.Tree
import GHC.Generics (Generic)

data ArgF a
  = Literal RawIdent
  | Var RawIdent
  | Str T.Text
  | DefiniteDescription DD.DefiniteDescription
  | ArgNode ([a], Maybe RawIdent)
  | ArgList ([a], Maybe RawIdent)
  deriving (Generic)

type Arg = Cofree ArgF Hint

instance Binary a => Binary (ArgF a)

instance Binary a => Binary (Cofree ArgF a)

type Args =
  ([Arg], Maybe RawIdent)

type Rules =
  Map.HashMap DD.DefiniteDescription (Hint, [(Args, Tree)])

type Sub =
  Map.HashMap RawIdent Tree

type MacroInfo =
  (Hint, DD.DefiniteDescription, [(Args, Tree)])

type PreMacroInfo =
  (Hint, DD.DefiniteDescription, [([Tree], Tree)])

showArg :: Arg -> T.Text
showArg arg =
  case arg of
    _ :< Literal sym ->
      "'" <> sym
    _ :< Var var ->
      var
    _ :< DefiniteDescription dd ->
      DD.reify dd
    _ :< Str str ->
      "\"" <> str <> "\""
    _ :< ArgNode args ->
      "(" <> showArgs args <> ")"
    _ :< ArgList args ->
      "[" <> showArgs args <> "]"

showArgs :: Args -> T.Text
showArgs (argList, mRest) =
  case mRest of
    Nothing ->
      T.intercalate " " (map showArg argList)
    Just rest ->
      T.intercalate " " (map showArg argList) <> " " <> rest
