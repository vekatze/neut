module Kernel.Common.Allocator
  ( Allocator (..),
    AllocatorSpec (..),
    defaultAllocator,
    showAllocator,
    allocatorSpec,
    mimallocArchive,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Hashable
import Data.Text qualified as T
import GHC.Generics (Generic)

data Allocator
  = Mimalloc
  | System
  deriving (Show, Eq, Generic)

instance Hashable Allocator

defaultAllocator :: Allocator
defaultAllocator =
  System

showAllocator :: Allocator -> T.Text
showAllocator allocator =
  case allocator of
    Mimalloc ->
      "mimalloc"
    System ->
      "system"

mimallocArchive :: ByteString
mimallocArchive =
  $(makeRelativeToProject "cache/mimalloc/out/release/libmimalloc.a" >>= embedFile)

data AllocatorSpec = AllocatorSpec
  { mallocName :: T.Text,
    reallocName :: T.Text,
    callocName :: T.Text,
    freeName :: T.Text
  }

allocatorSpec :: Allocator -> AllocatorSpec
allocatorSpec allocator =
  case allocator of
    Mimalloc ->
      AllocatorSpec
        { mallocName = "mi_malloc",
          reallocName = "mi_realloc",
          callocName = "mi_calloc",
          freeName = "mi_free"
        }
    System ->
      AllocatorSpec
        { mallocName = "malloc",
          reallocName = "realloc",
          callocName = "calloc",
          freeName = "free"
        }
