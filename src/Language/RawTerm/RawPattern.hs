module Language.RawTerm.RawPattern
  ( RawPattern (..),
    MarkedPattern,
    RawPatternRow,
    ConsArgs (..),
    TypeParams,
  )
where

import Language.Common.CallSite (IsSourceArg)
import Language.Common.Rune qualified as RU
import Language.RawTerm.Key
import Language.RawTerm.Name
import Language.RawTerm.RawIdent
import Logger.Hint
import SyntaxTree.C
import SyntaxTree.Series qualified as SE
import Language.Common.VarKind

data RawPattern
  = Var VarKind Name
  | Cons Name C (Maybe TypeParams) ConsArgs
  | RuneIntro RU.Rune

type MarkedPattern =
  (RawPattern, IsSourceArg)

type TypeParams =
  SE.Series (Hint, RawIdent)

data ConsArgs
  = Paren (SE.Series (Hint, MarkedPattern))
  | Of (SE.Series (Key, (Hint, C, MarkedPattern)))
  | Omitted

type RawPatternRow a =
  (SE.Series (Hint, RawPattern), C, a)
