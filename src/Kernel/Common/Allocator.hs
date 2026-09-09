module Kernel.Common.Allocator
  ( Allocator (..),
    AllocatorSpec (..),
    AllocatorKind (..),
    defaultAllocator,
    showAllocator,
    allocatorSpec,
    allocatorFamily,
    allocatorForeignList,
    allocatorLinkOption,
    mimallocArchive,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Hashable
import Data.Text qualified as T
import GHC.Generics (Generic)
import Kernel.Common.Arch qualified as Arch
import Language.Common.BaseLowType qualified as BLT
import Language.Common.BasePrimType qualified as BPT
import Language.Common.DataSize qualified as DS
import Language.Common.ExternalName qualified as EN
import Language.Common.Foreign qualified as F
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.PrimNumSize (dataSizeToIntSize)
import Logger.Hint (internalHint)

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

mimallocArchive :: Arch.Arch -> ByteString
mimallocArchive arch =
  case arch of
    Arch.Wasm32 ->
      mimallocArchiveWasm32
    _ ->
      mimallocArchiveHost

mimallocArchiveHost :: ByteString
mimallocArchiveHost =
  $(makeRelativeToProject "cache/mimalloc/out/release/libmimalloc.a" >>= embedFile)

mimallocArchiveWasm32 :: ByteString
mimallocArchiveWasm32 =
  $(makeRelativeToProject "cache/mimalloc/out/wasm32/libmimalloc.a" >>= embedFile)

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

allocatorLinkOption :: Arch.Arch -> Allocator -> [String]
allocatorLinkOption arch allocator =
  case (arch, allocator) of
    (Arch.Wasm32, Mimalloc) ->
      ["-lwasi-emulated-process-clocks"]
    _ ->
      []

allocatorFamily :: Allocator -> T.Text
allocatorFamily allocator =
  case allocator of
    Mimalloc ->
      "mimalloc"
    System ->
      "malloc"

data AllocatorKind
  = Malloc
  | Calloc
  | Realloc
  | Free
  deriving (Eq, Show)

allocatorForeignList :: DS.DataSize -> AllocatorSpec -> [(AllocatorKind, F.Foreign)]
allocatorForeignList dataSize spec = do
  let word = BLT.PrimNum $ BPT.Int $ BPT.Explicit $ dataSizeToIntSize dataSize
  let ptr = BLT.Pointer
  [ (Malloc, F.Foreign internalHint (EN.ExternalName $ mallocName spec) (F.Function [word] (FCT.Cod ptr))),
    (Calloc, F.Foreign internalHint (EN.ExternalName $ callocName spec) (F.Function [word, word] (FCT.Cod ptr))),
    (Realloc, F.Foreign internalHint (EN.ExternalName $ reallocName spec) (F.Function [ptr, word] (FCT.Cod ptr))),
    (Free, F.Foreign internalHint (EN.ExternalName $ freeName spec) (F.Function [ptr] FCT.Void))
    ]
