module Language.RawTerm.RawPattern
  ( RawPattern (..),
    MarkedPattern,
    RawPatternRow,
    ConsArgs (..),
    ImpBinders,
  )
where

import Language.Common.CallSite (IsSourceArg)
import Language.Common.Rune qualified as RU
import Language.Common.VarKind
import Language.RawTerm.Key
import Language.RawTerm.Name
import Language.RawTerm.RawIdent
import Logger.Hint
import SyntaxTree.C
import SyntaxTree.Series qualified as SE

data RawPattern
  = Var VarKind Name
  | Cons Name C (Maybe ImpBinders) ConsArgs
  | RuneIntro RU.Rune

type MarkedPattern =
  (RawPattern, IsSourceArg)

type ImpBinders =
  SE.Series (Hint, RawIdent)

data ConsArgs
  = Paren (SE.Series (Hint, MarkedPattern))
  | Of (SE.Series (Key, (Hint, C, MarkedPattern)))
  | Omitted

type RawPatternRow a =
  (SE.Series (Hint, RawPattern), C, a)
