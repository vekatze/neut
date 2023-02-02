module Entity.LowComp.Reduce
  ( reduce,
    Context (),
  )
where

import qualified Context.Env as Env
import qualified Context.Gensym as Gensym
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LowComp as LC
import Entity.LowComp.Subst
import qualified Entity.LowType as LT

type SizeMap =
  Map.Map LC.SizeInfo [(Int, LC.Value)]

class (Gensym.Context m, Env.Context m) => Context m

reduce :: Context m => SubstLowComp -> SizeMap -> LC.Comp -> m (S.Set Int, LC.Comp)
reduce sub sizeMap lowComp = do
  result <- reduce' sub sizeMap lowComp
  nopFreeSet <- Env.getNopFreeSet
  return (nopFreeSet, result)

reduce' :: Context m => SubstLowComp -> SizeMap -> LC.Comp -> m LC.Comp
reduce' sub sizeMap lowComp = do
  cancelAllocFlag <- Env.getShouldCancelAlloc
  case lowComp of
    LC.Return d ->
      return $ LC.Return $ substLowValue sub d
    LC.Let x op cont ->
      case op of
        LC.Bitcast d from to
          | from == to -> do
              let sub' = IntMap.insert (Ident.toInt x) (substLowValue sub d) sub
              reduce' sub' sizeMap cont
        LC.Alloc _ (LT.Pointer (LT.Array 0 _)) -> do
          let sub' = IntMap.insert (Ident.toInt x) LC.Null sub
          reduce' sub' sizeMap cont
        LC.Alloc _ (LT.Pointer (LT.Struct [])) -> do
          let sub' = IntMap.insert (Ident.toInt x) LC.Null sub
          reduce' sub' sizeMap cont
        LC.Alloc _ size
          | cancelAllocFlag,
            Just ((j, d) : rest) <- Map.lookup size sizeMap -> do
              Env.insertToNopFreeSet j
              let sizeMap' = Map.insert size rest sizeMap
              let sub' = IntMap.insert (Ident.toInt x) (substLowValue sub d) sub
              reduce' sub' sizeMap' cont
        _ -> do
          x' <- Gensym.newIdentFromIdent x
          let sub' = IntMap.insert (Ident.toInt x) (LC.VarLocal x') sub
          cont' <- reduce' sub' sizeMap cont
          return $ LC.Let x' (substOp sub op) cont'
    LC.Cont op@(LC.Free d size j) cont -> do
      let op' = substOp sub op
      let sizeMap' = Map.insertWith (++) size [(j, d)] sizeMap
      cont' <- reduce' sub sizeMap' cont
      return $ LC.Cont op' cont'
    LC.Cont op cont -> do
      let op' = substOp sub op
      cont' <- reduce' sub sizeMap cont
      return $ LC.Cont op' cont'
    LC.Switch (d, t) defaultBranch les -> do
      let d' = substLowValue sub d
      let (ls, es) = unzip les
      defaultBranch' <- reduce' sub sizeMap defaultBranch
      es' <- mapM (reduce' sub sizeMap) es
      return $ LC.Switch (d', t) defaultBranch' (zip ls es')
    LC.TailCall d ds -> do
      let d' = substLowValue sub d
      let ds' = map (substLowValue sub) ds
      return $ LC.TailCall d' ds'
    LC.Unreachable ->
      return LC.Unreachable
