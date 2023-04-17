module Entity.FilePos where

import Data.Binary
import Entity.Hint
import GHC.Generics

data FilePos
  = FilePos FilePath Loc
  deriving (Generic)

instance Binary FilePos

fromHint :: Hint -> FilePos
fromHint m =
  FilePos (metaFileName m) (metaLocation m)

showFilePos :: FilePos -> String
showFilePos (FilePos path (l, c)) =
  path ++ ":" ++ show l ++ ":" ++ show c

instance Show FilePos where
  show = showFilePos
