module Language.Comp.Switcher
  ( discardSwitch,
    copySwitch,
    sizeSwitch,
    defaultLabelSwitch,
    switchValue,
    directSize,
  )
where

import Language.Common.SlotSize (slotIntSize)
import Language.Comp.Comp qualified as C

discardSwitch :: Integer
discardSwitch =
  0

copySwitch :: Integer
copySwitch =
  1

sizeSwitch :: Integer
sizeSwitch =
  2

defaultLabelSwitch :: Int -> Integer
defaultLabelSwitch index =
  3 + toInteger index

switchValue :: Integer -> C.Value
switchValue =
  C.Int slotIntSize

directSize :: Integer
directSize =
  -1
