module Entity.LowComp.Subst
  ( substLowOp,
    substLowValue,
    SubstLowComp,
  )
where

import qualified Data.IntMap as IntMap
import Entity.Ident.Reify
import Entity.LowComp

type SubstLowComp =
  IntMap.IntMap LowValue

substLowOp :: SubstLowComp -> LowOp -> LowOp
substLowOp sub llvmOp =
  case llvmOp of
    LowOpCall d ds -> do
      let d' = substLowValue sub d
      let ds' = map (substLowValue sub) ds
      LowOpCall d' ds'
    LowOpGetElementPtr (d, t) dts -> do
      let d' = substLowValue sub d
      let (ds, ts) = unzip dts
      let ds' = map (substLowValue sub) ds
      LowOpGetElementPtr (d', t) (zip ds' ts)
    LowOpBitcast d t1 t2 -> do
      let d' = substLowValue sub d
      LowOpBitcast d' t1 t2
    LowOpIntToPointer d t1 t2 -> do
      let d' = substLowValue sub d
      LowOpIntToPointer d' t1 t2
    LowOpPointerToInt d t1 t2 -> do
      let d' = substLowValue sub d
      LowOpPointerToInt d' t1 t2
    LowOpLoad d t -> do
      let d' = substLowValue sub d
      LowOpLoad d' t
    LowOpStore t d1 d2 -> do
      let d1' = substLowValue sub d1
      let d2' = substLowValue sub d2
      LowOpStore t d1' d2'
    LowOpAlloc d sizeInfo -> do
      let d' = substLowValue sub d
      LowOpAlloc d' sizeInfo
    LowOpFree d sizeInfo i -> do
      let d' = substLowValue sub d
      LowOpFree d' sizeInfo i
    LowOpPrimOp op ds -> do
      let ds' = map (substLowValue sub) ds
      LowOpPrimOp op ds'
    LowOpSyscall i ds -> do
      let ds' = map (substLowValue sub) ds
      LowOpSyscall i ds'

substLowValue :: SubstLowComp -> LowValue -> LowValue
substLowValue sub llvmValue =
  case llvmValue of
    LowValueVarLocal x ->
      case IntMap.lookup (toInt x) sub of
        Just d ->
          d
        Nothing ->
          LowValueVarLocal x
    _ ->
      llvmValue
