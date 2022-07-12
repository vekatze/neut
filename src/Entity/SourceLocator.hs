{-# LANGUAGE TemplateHaskell #-}

module Entity.SourceLocator
  ( SourceLocator (..),
    toText,
    reflect,
    bottomLocator,
    topLocator,
    boolLocator,
    llvmLocator,
  )
where

import Control.Exception.Safe
import Data.Binary
import Data.Hashable
import qualified Data.Text as T
import GHC.Generics
import Path

newtype SourceLocator = SourceLocator {reify :: Path Rel File}
  deriving (Generic, Eq, Show)

instance Hashable SourceLocator

instance Binary SourceLocator where
  put (SourceLocator v) = put $ toFilePath v
  get = do
    filePath <- get
    path <- case parseRelFile filePath of
      Just path ->
        return path
      Nothing ->
        fail $ "couldn't parse given path: " <> filePath
    return $ SourceLocator path

-- reify :: SourceLocator -> T.Text
-- reify (SourceLocator sl) =
--   T.replace "/" "." $ T.pack $ toFilePath sl

-- fixme: parametrize "/"
toText :: SourceLocator -> T.Text
toText (SourceLocator sl) =
  T.replace "/" "." $ T.pack $ toFilePath sl

-- fixme: parametrize "/"
reflect :: MonadThrow m => T.Text -> m SourceLocator
reflect rawTxt = do
  path <- parseRelFile $ T.unpack $ T.replace "." "/" rawTxt
  return $ SourceLocator path

bottomLocator :: SourceLocator
bottomLocator =
  SourceLocator $(mkRelFile "bottom")

topLocator :: SourceLocator
topLocator =
  SourceLocator $(mkRelFile "top")

boolLocator :: SourceLocator
boolLocator =
  SourceLocator $(mkRelFile "bool")

llvmLocator :: SourceLocator
llvmLocator =
  SourceLocator $(mkRelFile "llvm")
