module Library.Logger.Rule.Handle
  ( Handle (..),
    _formatNominalDiffTime,
  )
where

import Data.Text qualified as T
import Data.Time
import Library.Color.Rule.Handle qualified as Color
import Text.Printf (printf)

data Handle = InternalHandle
  { _colorHandle :: Color.Handle,
    _endOfEntry :: T.Text,
    _enableDebugMode :: Bool,
    _baseTime :: UTCTime
  }

_formatNominalDiffTime :: NominalDiffTime -> String
_formatNominalDiffTime t =
  printf "%.6f" (realToFrac t :: Double)
