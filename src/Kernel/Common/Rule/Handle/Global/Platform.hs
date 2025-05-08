module Kernel.Common.Rule.Handle.Global.Platform
  ( Handle (..),
    getArch,
    getDataSizeValue,
    getDataSize,
    getPlatform,
    getClangDigest,
  )
where

import Data.Text qualified as T
import Kernel.Common.Rule.Arch qualified as Arch
import Kernel.Common.Rule.OS qualified as O
import Kernel.Common.Rule.Platform qualified as P
import Language.Common.Rule.DataSize qualified as DS

data Handle = Handle
  { _arch :: Arch.Arch,
    _os :: O.OS,
    _clangDigest :: T.Text,
    _baseSize :: DS.DataSize
  }

getDataSizeValue :: Handle -> Int
getDataSizeValue h =
  DS.reify $ _baseSize h

getArch :: Handle -> Arch.Arch
getArch =
  _arch

getOS :: Handle -> O.OS
getOS =
  _os

getPlatform :: Handle -> P.Platform
getPlatform h = do
  let arch = getArch h
  let os = getOS h
  P.Platform {arch, os}

getDataSize :: Handle -> DS.DataSize
getDataSize =
  _baseSize

getClangDigest :: Handle -> T.Text
getClangDigest = do
  _clangDigest
