module Kernel.Common.Rule.TopCandidate
  ( TopCandidate (..),
    CandidateKind (..),
  )
where

import Aux.Logger.Rule.Hint
import Data.Binary
import GHC.Generics
import Language.Common.Rule.DefiniteDescription qualified as DD

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
