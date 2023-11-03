module Entity.Hint where

import Data.Binary
import GHC.Generics

data Hint = Hint
  { metaFileName :: FilePath,
    metaLocation :: Loc
  }
  deriving (Generic)

type Line =
  Int

type Column =
  Int

type Loc =
  (Line, Column)

instance Binary Hint where
  put _ = put ()
  get = return internalHint

instance Show Hint where
  show _ =
    "_"

instance Ord Hint where
  _ `compare` _ = EQ

instance Eq Hint where
  _ == _ = True

new :: Int -> Int -> FilePath -> Hint
new l c path =
  Hint
    { metaFileName = path,
      metaLocation = (l, c)
    }

internalHint :: Hint
internalHint =
  Hint
    { metaFileName = "",
      metaLocation = (0, 0)
    }
