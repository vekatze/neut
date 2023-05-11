module Entity.LowComp.Subst
  ( substOp,
    substLowValue,
    SubstLowComp,
  )
where

import Data.IntMap qualified as IntMap
import Entity.Ident.Reify
import Entity.LowComp qualified as LC

type SubstLowComp =
  IntMap.IntMap LC.Value

substOp :: SubstLowComp -> LC.Op -> LC.Op
substOp sub llvmOp =
  case llvmOp of
    LC.Call d ds -> do
      let d' = substLowValue sub d
      let ds' = map (substLowValue sub) ds
      LC.Call d' ds'
    LC.GetElementPtr (d, t) dts -> do
      let d' = substLowValue sub d
      let (ds, ts) = unzip dts
      let ds' = map (substLowValue sub) ds
      LC.GetElementPtr (d', t) (zip ds' ts)
    LC.Bitcast d t1 t2 -> do
      let d' = substLowValue sub d
      LC.Bitcast d' t1 t2
    LC.IntToPointer d t1 t2 -> do
      let d' = substLowValue sub d
      LC.IntToPointer d' t1 t2
    LC.PointerToInt d t1 t2 -> do
      let d' = substLowValue sub d
      LC.PointerToInt d' t1 t2
    LC.Load d t -> do
      let d' = substLowValue sub d
      LC.Load d' t
    LC.Store t d1 d2 -> do
      let d1' = substLowValue sub d1
      let d2' = substLowValue sub d2
      LC.Store t d1' d2'
    LC.Alloc d -> do
      let d' = substLowValue sub d
      LC.Alloc d'
    LC.Free d -> do
      LC.Free $ substLowValue sub d
    LC.PrimOp op ds -> do
      let ds' = map (substLowValue sub) ds
      LC.PrimOp op ds'
    LC.Syscall i ds -> do
      let ds' = map (substLowValue sub) ds
      LC.Syscall i ds'

substLowValue :: SubstLowComp -> LC.Value -> LC.Value
substLowValue sub llvmValue =
  case llvmValue of
    LC.VarLocal x ->
      case IntMap.lookup (toInt x) sub of
        Just d ->
          d
        Nothing ->
          LC.VarLocal x
    _ ->
      llvmValue
