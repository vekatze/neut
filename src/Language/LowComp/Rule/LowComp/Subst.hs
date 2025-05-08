module Language.LowComp.Rule.LowComp.Subst
  ( substOp,
    substLowValue,
    SubstLowComp,
  )
where

import Data.IntMap qualified as IntMap
import Language.Common.Rule.Ident.Reify
import Language.LowComp.Rule.LowComp qualified as LC

type SubstLowComp =
  IntMap.IntMap LC.Value

substOp :: SubstLowComp -> LC.Op -> LC.Op
substOp sub llvmOp =
  case llvmOp of
    LC.Call codType d tds -> do
      let d' = substLowValue sub d
      let (ts, ds) = unzip tds
      let ds' = map (substLowValue sub) ds
      LC.Call codType d' (zip ts ds')
    LC.MagicCall funcType d tds -> do
      let d' = substLowValue sub d
      let (ts, ds) = unzip tds
      let ds' = map (substLowValue sub) ds
      LC.MagicCall funcType d' (zip ts ds')
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
    LC.StackAlloc lt indexType num -> do
      let num' = substLowValue sub num
      LC.StackAlloc lt indexType num'
    LC.Alloc d size allocID -> do
      let d' = substLowValue sub d
      LC.Alloc d' size allocID
    LC.Free d size freeID -> do
      LC.Free (substLowValue sub d) size freeID
    LC.PrimOp op ds -> do
      let ds' = map (substLowValue sub) ds
      LC.PrimOp op ds'

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
