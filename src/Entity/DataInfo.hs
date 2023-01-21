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
    constCoproduct,
    constCoproductLeft,
    constCoproductRight,
  )
where

import Data.Binary
import Entity.BaseName qualified as BN
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.LocalLocator qualified as LL
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
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

{-# INLINE constCoproduct #-}
constCoproduct :: DD.DefiniteDescription
constCoproduct =
  DD.newByGlobalLocator (SGL.baseGlobalLocatorOf SL.coproductLocator) [] BN.coproduct

{-# INLINE constCoproductLeft #-}
constCoproductLeft :: DD.DefiniteDescription
constCoproductLeft =
  DD.extendLL constCoproduct $ LL.new [] BN.coproductLeft

{-# INLINE constCoproductRight #-}
constCoproductRight :: DD.DefiniteDescription
constCoproductRight =
  DD.extendLL constCoproduct $ LL.new [] BN.coproductRight
