module Logger.Handle
  ( Handle (..),
    _formatNominalDiffTime,
    setModuleDir,
  )
where

import Color.Handle qualified as Color
import Data.IORef
import Data.Text qualified as T
import Data.Time
import Kernel.Common.Module (MainModule, extractModule, moduleLocation)
import Path (parent, toFilePath)
import Text.Printf (printf)

data Handle = InternalHandle
  { _colorHandle :: Color.Handle,
    _enableDebugMode :: Bool,
    _baseTime :: UTCTime,
    _moduleDirRef :: IORef T.Text
  }

_formatNominalDiffTime :: NominalDiffTime -> String
_formatNominalDiffTime t =
  printf "%.6f" (realToFrac t :: Double)

setModuleDir :: Handle -> MainModule -> IO ()
setModuleDir h mainModule = do
  let moduleDir = T.pack $ toFilePath $ parent $ moduleLocation $ extractModule mainModule
  writeIORef (_moduleDirRef h) moduleDir
