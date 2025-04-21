module Rule.TopCandidate
  ( TopCandidate (..),
    CandidateKind (..),
  )
where

import Data.Binary
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import GHC.Generics

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
