module Context.LLVM
  ( Context (..),
  )
where

import Data.ByteString.Lazy qualified as L
import Entity.OutputKind
import Path

type OutputPath = Path Abs File

class Monad m => Context m where
  emit :: [OutputKind] -> L.ByteString -> m ()
  link :: String -> [Path Abs File] -> OutputPath -> m ()
