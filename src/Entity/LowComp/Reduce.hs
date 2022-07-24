module Entity.LowComp.Reduce
  ( reduce,
    Context (..),
  )
where

import qualified Context.App as App
import qualified Context.Gensym as Gensym
import Data.IORef
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Entity.Ident.Reify as Ident
import Entity.LowComp
import Entity.LowComp.Subst
import Entity.LowType

type SizeMap =
  Map.Map SizeInfo [(Int, LowValue)]

data Context = Context
  { shouldCancelAlloc :: Bool,
    gensym :: Gensym.Context,
    nopFreeSetRef :: IORef (S.Set Int)
  }

reduce :: App.Context -> SubstLowComp -> SizeMap -> LowComp -> IO (S.Set Int, LowComp)
reduce ctx sub sizeMap lowComp = do
  ctx' <- specialize ctx
  result <- reduce' ctx' sub sizeMap lowComp
  nopFreeSet <- readIORef $ nopFreeSetRef ctx'
  return (nopFreeSet, result)

specialize :: App.Context -> IO Context
specialize ctx = do
  _nopFreeSetRef <- newIORef S.empty
  return
    Context
      { shouldCancelAlloc = App.shouldCancelAlloc ctx,
        gensym = App.gensym ctx,
        nopFreeSetRef = _nopFreeSetRef
      }

reduce' :: Context -> SubstLowComp -> SizeMap -> LowComp -> IO LowComp
reduce' ctx sub sizeMap lowComp = do
  let cancelAllocFlag = shouldCancelAlloc ctx
  case lowComp of
    LowCompReturn d ->
      return $ LowCompReturn $ substLowValue sub d
    LowCompLet x op cont ->
      case op of
        LowOpBitcast d from to
          | from == to -> do
            let sub' = IntMap.insert (Ident.toInt x) (substLowValue sub d) sub
            reduce' ctx sub' sizeMap cont
        LowOpAlloc _ (LowTypePointer (LowTypeArray 0 _)) -> do
          let sub' = IntMap.insert (Ident.toInt x) LowValueNull sub
          reduce' ctx sub' sizeMap cont
        LowOpAlloc _ (LowTypePointer (LowTypeStruct [])) -> do
          let sub' = IntMap.insert (Ident.toInt x) LowValueNull sub
          reduce' ctx sub' sizeMap cont
        LowOpAlloc _ size
          | cancelAllocFlag,
            Just ((j, d) : rest) <- Map.lookup size sizeMap -> do
            modifyIORef' (nopFreeSetRef ctx) $ S.insert j
            let sizeMap' = Map.insert size rest sizeMap
            let sub' = IntMap.insert (Ident.toInt x) (substLowValue sub d) sub
            reduce' ctx sub' sizeMap' cont
        _ -> do
          x' <- Gensym.newIdentFromIdent (gensym ctx) x
          let sub' = IntMap.insert (Ident.toInt x) (LowValueVarLocal x') sub
          cont' <- reduce' ctx sub' sizeMap cont
          return $ LowCompLet x' (substLowOp sub op) cont'
    LowCompCont op@(LowOpFree d size j) cont -> do
      let op' = substLowOp sub op
      let sizeMap' = Map.insertWith (++) size [(j, d)] sizeMap
      cont' <- reduce' ctx sub sizeMap' cont
      return $ LowCompCont op' cont'
    LowCompCont op cont -> do
      let op' = substLowOp sub op
      cont' <- reduce' ctx sub sizeMap cont
      return $ LowCompCont op' cont'
    LowCompSwitch (d, t) defaultBranch les -> do
      let d' = substLowValue sub d
      let (ls, es) = unzip les
      defaultBranch' <- reduce' ctx sub sizeMap defaultBranch
      es' <- mapM (reduce' ctx sub sizeMap) es
      return $ LowCompSwitch (d', t) defaultBranch' (zip ls es')
    LowCompCall d ds -> do
      let d' = substLowValue sub d
      let ds' = map (substLowValue sub) ds
      return $ LowCompCall d' ds'
    LowCompUnreachable ->
      return LowCompUnreachable
