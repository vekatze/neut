module Rule.FilePos
  ( FilePos (..),
    fromHint,
    showFilePos,
  )
where

import Data.Binary
import Rule.Hint
import GHC.Generics
import Path

data FilePos
  = FilePos (Path Abs File) Loc
  deriving (Generic)

instance Binary FilePos where
  put (FilePos path loc) = do
    put $ toFilePath path
    put loc
  get = do
    filePath <- get
    loc <- get
    path <- case parseAbsFile filePath of
      Just path ->
        return path
      Nothing ->
        fail $ "Could not parse given path: " <> filePath
    return $ FilePos path loc

fromHint :: Hint -> Maybe FilePos
fromHint m = do
  path <- parseAbsFile $ metaFileName m
  return $ FilePos path (metaLocation m)

showFilePos :: FilePos -> String
showFilePos (FilePos path (l, c)) =
  toFilePath path ++ ":" ++ show l ++ ":" ++ show c

instance Show FilePos where
  show = showFilePos
