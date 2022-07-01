module Context.LLVM
  ( Axis (..),
  )
where

import qualified Data.ByteString.Lazy as L
import Entity.OutputKind
import Path

type OutputPath = Path Abs File

data Axis = Axis
  { emit :: OutputKind -> L.ByteString -> OutputPath -> IO (),
    link :: [Path Abs File] -> OutputPath -> IO ()
  }
