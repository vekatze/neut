module Entity.LowComp.Reduce
  ( reduceLowComp,
  )
where

import Context.Gensym
import Data.IORef
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as S
import Entity.Global
import qualified Entity.Ident.Reify as Ident
import Entity.LowComp
import Entity.LowComp.Subst
import Entity.LowType

type SizeMap =
  Map.Map SizeInfo [(Int, LowValue)]

reduceLowComp :: Axis -> SubstLowComp -> SizeMap -> LowComp -> IO LowComp
reduceLowComp axis sub sizeMap llvm = do
  cancelAllocFlag <- readIORef shouldCancelAllocRef
  case llvm of
    LowCompReturn d ->
      return $ LowCompReturn $ substLowValue sub d
    LowCompLet x op cont ->
      case op of
        LowOpBitcast d from to
          | from == to -> do
            let sub' = IntMap.insert (Ident.toInt x) (substLowValue sub d) sub
            reduceLowComp axis sub' sizeMap cont
        LowOpAlloc _ (LowTypePointer (LowTypeArray 0 _)) -> do
          let sub' = IntMap.insert (Ident.toInt x) LowValueNull sub
          reduceLowComp axis sub' sizeMap cont
        LowOpAlloc _ (LowTypePointer (LowTypeStruct [])) -> do
          let sub' = IntMap.insert (Ident.toInt x) LowValueNull sub
          reduceLowComp axis sub' sizeMap cont
        LowOpAlloc _ size
          | cancelAllocFlag,
            Just ((j, d) : rest) <- Map.lookup size sizeMap -> do
            modifyIORef' nopFreeSetRef $ S.insert j
            let sizeMap' = Map.insert size rest sizeMap
            let sub' = IntMap.insert (Ident.toInt x) (substLowValue sub d) sub
            reduceLowComp axis sub' sizeMap' cont
        _ -> do
          x' <- newIdentFromIdent axis x
          let sub' = IntMap.insert (Ident.toInt x) (LowValueVarLocal x') sub
          cont' <- reduceLowComp axis sub' sizeMap cont
          return $ LowCompLet x' (substLowOp sub op) cont'
    LowCompCont op@(LowOpFree d size j) cont -> do
      let op' = substLowOp sub op
      let sizeMap' = Map.insertWith (++) size [(j, d)] sizeMap
      cont' <- reduceLowComp axis sub sizeMap' cont
      return $ LowCompCont op' cont'
    LowCompCont op cont -> do
      let op' = substLowOp sub op
      cont' <- reduceLowComp axis sub sizeMap cont
      return $ LowCompCont op' cont'
    LowCompSwitch (d, t) defaultBranch les -> do
      let d' = substLowValue sub d
      let (ls, es) = unzip les
      defaultBranch' <- reduceLowComp axis sub sizeMap defaultBranch
      es' <- mapM (reduceLowComp axis sub sizeMap) es
      return $ LowCompSwitch (d', t) defaultBranch' (zip ls es')
    LowCompCall d ds -> do
      let d' = substLowValue sub d
      let ds' = map (substLowValue sub) ds
      return $ LowCompCall d' ds'
    LowCompUnreachable ->
      return LowCompUnreachable
