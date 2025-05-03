module Rule.FilePos
  ( FilePos (..),
    Line,
    Column,
    Loc,
    showFilePos,
  )
where

import Data.Binary
import GHC.Generics
import Path

type Line =
  Int

type Column =
  Int

type Loc =
  (Line, Column)

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

instance Show FilePos where
  show = showFilePos

showFilePos :: FilePos -> String
showFilePos (FilePos path (l, c)) =
  toFilePath path ++ ":" ++ show l ++ ":" ++ show c
