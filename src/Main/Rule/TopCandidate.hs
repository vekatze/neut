module Main.Rule.TopCandidate
  ( TopCandidate (..),
    CandidateKind (..),
  )
where

import Data.Binary
import GHC.Generics
import Main.Rule.DefiniteDescription qualified as DD
import Main.Rule.Hint

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
