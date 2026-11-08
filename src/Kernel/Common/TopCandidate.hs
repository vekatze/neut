module Kernel.Common.TopCandidate
  ( TopCandidate (..),
    CandidateKind (..),
  )
where

import Binary.Derive (deriveBinary)
import Language.Common.DefiniteDescription qualified as DD
import Logger.Hint

data TopCandidate = TopCandidate
  { loc :: Loc,
    dd :: DD.DefiniteDescription,
    kind :: CandidateKind
  }

data CandidateKind
  = Constant
  | Constructor
  | Function

deriveBinary [''TopCandidate, ''CandidateKind]
