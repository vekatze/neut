module Language.Common.SourceLocator
  ( SourceLocator (..),
    getRelPathText,
    toText,
    fromBaseNameList,
    llvmLocator,
    internalLocator,
    typeTagLocator,
    binaryLocator,
    vectorLocator,
    eitherLocator,
    pairLocator,
    unitLocator,
    textLocator,
  )
where

import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import GHC.Generics
import Language.Common.BaseName qualified as BN
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
        fail $ "Could not parse given path: " <> filePath
    return $ SourceLocator path

-- fixme: parametrize "/"
toText :: SourceLocator -> T.Text
toText (SourceLocator sl) =
  T.replace "/" "." $ T.pack $ toFilePath sl

fromBaseNameList :: [BN.BaseName] -> Maybe SourceLocator
fromBaseNameList baseNameList = do
  path <- parseRelFile $ T.unpack $ T.intercalate "/" $ map BN.reify baseNameList
  return $ SourceLocator path

getRelPathText :: SourceLocator -> T.Text
getRelPathText sl =
  T.pack $ toFilePath $ reify sl

llvmLocator :: SourceLocator
llvmLocator =
  SourceLocator $(mkRelFile "llvm")

internalLocator :: SourceLocator
internalLocator =
  SourceLocator $(mkRelFile "#")

typeTagLocator :: SourceLocator
typeTagLocator =
  SourceLocator $(mkRelFile "type-tag")

binaryLocator :: SourceLocator
binaryLocator =
  SourceLocator $(mkRelFile "binary")

vectorLocator :: SourceLocator
vectorLocator =
  SourceLocator $(mkRelFile "vector")

eitherLocator :: SourceLocator
eitherLocator =
  SourceLocator $(mkRelFile "either")

pairLocator :: SourceLocator
pairLocator =
  SourceLocator $(mkRelFile "pair")

unitLocator :: SourceLocator
unitLocator =
  SourceLocator $(mkRelFile "unit")

textLocator :: SourceLocator
textLocator =
  SourceLocator $(mkRelFile "text")
