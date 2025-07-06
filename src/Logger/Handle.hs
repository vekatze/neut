module Logger.Handle
  ( Handle (..),
    _formatNominalDiffTime,
  )
where

import Color.Handle qualified as Color
import Data.Time
import Text.Printf (printf)

data Handle = InternalHandle
  { _colorHandle :: Color.Handle,
    _enableDebugMode :: Bool,
    _baseTime :: UTCTime
  }

_formatNominalDiffTime :: NominalDiffTime -> String
_formatNominalDiffTime t =
  printf "%.6f" (realToFrac t :: Double)
