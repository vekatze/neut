module Entity.FilePos where

import Entity.Hint

data FilePos
  = FilePos FilePath Loc

fromHint :: Hint -> FilePos
fromHint m =
  FilePos (metaFileName m) (metaLocation m)

showFilePos :: FilePos -> String
showFilePos (FilePos path (l, c)) =
  path ++ ":" ++ show l ++ ":" ++ show c

instance Show FilePos where
  show = showFilePos
