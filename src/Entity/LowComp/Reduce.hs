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
import Entity.LowComp
import Entity.LowComp.Subst
import Entity.LowType

type SizeMap =
  Map.Map SizeInfo [(Int, LowValue)]

class (Gensym.Context m, Env.Context m) => Context m

reduce :: Context m => SubstLowComp -> SizeMap -> LowComp -> m (S.Set Int, LowComp)
reduce sub sizeMap lowComp = do
  result <- reduce' sub sizeMap lowComp
  nopFreeSet <- Env.getNopFreeSet
  return (nopFreeSet, result)

reduce' :: Context m => SubstLowComp -> SizeMap -> LowComp -> m LowComp
reduce' sub sizeMap lowComp = do
  cancelAllocFlag <- Env.getShouldCancelAlloc
  case lowComp of
    LowCompReturn d ->
      return $ LowCompReturn $ substLowValue sub d
    LowCompLet x op cont ->
      case op of
        LowOpBitcast d from to
          | from == to -> do
            let sub' = IntMap.insert (Ident.toInt x) (substLowValue sub d) sub
            reduce' sub' sizeMap cont
        LowOpAlloc _ (LowTypePointer (LowTypeArray 0 _)) -> do
          let sub' = IntMap.insert (Ident.toInt x) LowValueNull sub
          reduce' sub' sizeMap cont
        LowOpAlloc _ (LowTypePointer (LowTypeStruct [])) -> do
          let sub' = IntMap.insert (Ident.toInt x) LowValueNull sub
          reduce' sub' sizeMap cont
        LowOpAlloc _ size
          | cancelAllocFlag,
            Just ((j, d) : rest) <- Map.lookup size sizeMap -> do
            -- modifyIORef' nopFreeSetRef $ S.insert j
            Env.insertToNopFreeSet j
            let sizeMap' = Map.insert size rest sizeMap
            let sub' = IntMap.insert (Ident.toInt x) (substLowValue sub d) sub
            reduce' sub' sizeMap' cont
        _ -> do
          x' <- Gensym.newIdentFromIdent x
          let sub' = IntMap.insert (Ident.toInt x) (LowValueVarLocal x') sub
          cont' <- reduce' sub' sizeMap cont
          return $ LowCompLet x' (substLowOp sub op) cont'
    LowCompCont op@(LowOpFree d size j) cont -> do
      let op' = substLowOp sub op
      let sizeMap' = Map.insertWith (++) size [(j, d)] sizeMap
      cont' <- reduce' sub sizeMap' cont
      return $ LowCompCont op' cont'
    LowCompCont op cont -> do
      let op' = substLowOp sub op
      cont' <- reduce' sub sizeMap cont
      return $ LowCompCont op' cont'
    LowCompSwitch (d, t) defaultBranch les -> do
      let d' = substLowValue sub d
      let (ls, es) = unzip les
      defaultBranch' <- reduce' sub sizeMap defaultBranch
      es' <- mapM (reduce' sub sizeMap) es
      return $ LowCompSwitch (d', t) defaultBranch' (zip ls es')
    LowCompCall d ds -> do
      let d' = substLowValue sub d
      let ds' = map (substLowValue sub) ds
      return $ LowCompCall d' ds'
    LowCompUnreachable ->
      return LowCompUnreachable
