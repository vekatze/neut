module Rule.SourceLocator
  ( SourceLocator (..),
    getRelPathText,
    toText,
    fromBaseNameList,
    llvmLocator,
    internalLocator,
  )
where

import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import Rule.BaseName qualified as BN
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

llvmLocator :: SourceLocator
llvmLocator =
  SourceLocator $(mkRelFile "llvm")

internalLocator :: SourceLocator
internalLocator =
  SourceLocator $(mkRelFile "#")

getRelPathText :: SourceLocator -> T.Text
getRelPathText sl =
  T.pack $ toFilePath $ reify sl
