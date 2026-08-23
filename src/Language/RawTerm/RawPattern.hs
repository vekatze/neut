module Language.RawTerm.RawPattern
  ( RawPattern (..),
    MarkedPattern,
    RawPatternRow,
    ConsArgs (..),
  )
where

import Language.Common.CallSite (IsSourceArg)
import Language.Common.Rune qualified as RU
import Language.RawTerm.Key
import Language.RawTerm.Name
import Logger.Hint
import SyntaxTree.C
import SyntaxTree.Series qualified as SE
import Language.Common.VarKind

data RawPattern
  = Var VarKind Name
  | Cons Name C ConsArgs
  | RuneIntro RU.Rune

type MarkedPattern =
  (RawPattern, IsSourceArg)

data ConsArgs
  = Paren (SE.Series (Hint, MarkedPattern))
  | Of (SE.Series (Key, (Hint, C, MarkedPattern)))

type RawPatternRow a =
  (SE.Series (Hint, RawPattern), C, a)
