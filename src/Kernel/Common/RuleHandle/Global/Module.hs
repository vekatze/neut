module Kernel.Common.RuleHandle.Global.Module
  ( Handle (..),
    _hasSourceExtension,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Kernel.Common.Const
import Kernel.Common.Module
import Path

newtype Handle = Handle
  { _moduleCacheMapRef :: IORef (Map.HashMap (Path Abs File) Module)
  }

_hasSourceExtension :: Path Abs File -> Bool
_hasSourceExtension path =
  case splitExtension path of
    Just (_, ext)
      | ext == sourceFileExtension ->
          True
    _ ->
      False
