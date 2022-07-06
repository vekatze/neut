module Context.LLVM
  ( Context (..),
    Config (..),
  )
where

import qualified Context.Throw as Throw
import qualified Data.ByteString.Lazy as L
import Entity.OutputKind
import Path

type OutputPath = Path Abs File

data Context = Context
  { emit :: OutputKind -> L.ByteString -> OutputPath -> IO (),
    link :: [Path Abs File] -> OutputPath -> IO ()
  }

data Config = Config
  { clangOptString :: String,
    throwCtx :: Throw.Context
  }
