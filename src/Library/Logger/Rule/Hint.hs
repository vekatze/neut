module Library.Logger.Rule.Hint
  ( Hint (..),
    SavedHint (..),
    Line,
    Column,
    Loc,
    newHint,
    blur,
    internalHint,
    newSourceHint,
    fakeLoc,
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

data Hint = Hint
  { metaFileName :: FilePath,
    metaLocation :: Loc,
    metaShouldSaveLocation :: Bool
  }
  deriving (Generic)

instance Binary Hint where
  put _ =
    return ()
  get =
    return internalHint

instance Show Hint where
  show _ =
    "_"

instance Ord Hint where
  _ `compare` _ = EQ

instance Eq Hint where
  _ == _ = True

newtype SavedHint = SavedHint Hint deriving (Generic)

instance Show SavedHint where
  show (SavedHint m) = show m

instance Binary SavedHint where
  put (SavedHint val) = do
    put $ metaFileName val
    put $ metaLocation val
    put $ metaShouldSaveLocation val
  get = do
    SavedHint <$> (Hint <$> get <*> get <*> get)

newHint :: Int -> Int -> FilePath -> Hint
newHint l c path =
  Hint
    { metaFileName = path,
      metaLocation = (l, c),
      metaShouldSaveLocation = True
    }

blur :: Hint -> Hint
blur m =
  m {metaShouldSaveLocation = False}

internalHint :: Hint
internalHint =
  Hint
    { metaFileName = "",
      metaLocation = (1, 1),
      metaShouldSaveLocation = False
    }

newSourceHint :: Path Abs File -> Hint
newSourceHint path =
  newHint 1 1 $ toFilePath path

fakeLoc :: Loc
fakeLoc =
  (1, 1)

showFilePos :: Hint -> String
showFilePos (Hint {metaFileName, metaLocation = (l, c)}) =
  metaFileName ++ ":" ++ show l ++ ":" ++ show c
