module Kernel.Common.TopCandidate
  ( TopCandidate (..),
    CandidateKind (..),
  )
where

import Data.Binary
import GHC.Generics
import Language.Common.DefiniteDescription qualified as DD
import Logger.Hint

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
