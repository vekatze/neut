{-# LANGUAGE TemplateHaskell #-}

module Entity.SourceLocator
  ( SourceLocator (..),
    toText,
    fromBaseNameList,
    llvmLocator,
    internalLocator,
    natLocator,
  )
where

import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import Entity.BaseName qualified as BN
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

-- fixme: parametrize "/"
toText :: SourceLocator -> T.Text
toText (SourceLocator sl) =
  T.replace "/" "." $ T.pack $ toFilePath sl

fromBaseNameList :: [BN.BaseName] -> Maybe SourceLocator
fromBaseNameList baseNameList = do
  path <- parseRelFile $ T.unpack $ T.intercalate "/" $ map BN.reify baseNameList
  return $ SourceLocator path

llvmLocator :: SourceLocator
llvmLocator =
  SourceLocator $(mkRelFile "llvm")

internalLocator :: SourceLocator
internalLocator =
  SourceLocator $(mkRelFile "#")

natLocator :: SourceLocator
natLocator =
  SourceLocator $(mkRelFile "nat")
