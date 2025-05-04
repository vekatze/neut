module Main.Rule.TopCandidate
  ( TopCandidate (..),
    CandidateKind (..),
  )
where

import Data.Binary
import GHC.Generics
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Hint

data TopCandidate = TopCandidate
  { loc :: Loc,
    dd :: DD.DefiniteDescription,
    kind :: CandidateKind
  }
  deriving (Generic)

instance Binary TopCandidate

data CandidateKind
  = Constant
  | Constructor
  | Function
  deriving (Generic)

instance Binary CandidateKind
