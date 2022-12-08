module Entity.DataInfo
  ( DataInfo,
    DataValue,
    fromDataInfo,
    constBottom,
    constTop,
    constTopUnit,
    constBool,
    constBoolTrue,
    constBoolFalse,
  )
where

import Data.Binary
import qualified Entity.BaseName as BN
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import qualified Entity.LocalLocator as LL
import qualified Entity.SourceLocator as SL
import qualified Entity.StrictGlobalLocator as SGL
import GHC.Generics

type DataValue = (DD.DefiniteDescription, D.Discriminant)

newtype DataInfo = DataInfoCons {fromDataInfo :: (DD.DefiniteDescription, [DataValue])}
  deriving (Generic, Show)

instance Binary DataInfo

constBottom :: DD.DefiniteDescription
constBottom =
  DD.newByGlobalLocator (SGL.baseGlobalLocatorOf SL.bottomLocator) [] BN.bottom

constTop :: DD.DefiniteDescription
constTop =
  DD.newByGlobalLocator (SGL.baseGlobalLocatorOf SL.topLocator) [] BN.top

constTopUnit :: DD.DefiniteDescription
constTopUnit =
  DD.extendLL constTop $ LL.new [] BN.topUnit

{-# INLINE constBool #-}
constBool :: DD.DefiniteDescription
constBool =
  DD.newByGlobalLocator (SGL.baseGlobalLocatorOf SL.boolLocator) [] BN.bool

{-# INLINE constBoolTrue #-}
constBoolTrue :: DD.DefiniteDescription
constBoolTrue =
  DD.extendLL constBool $ LL.new [] BN.boolTrue

{-# INLINE constBoolFalse #-}
constBoolFalse :: DD.DefiniteDescription
constBoolFalse =
  DD.extendLL constBool $ LL.new [] BN.boolFalse
