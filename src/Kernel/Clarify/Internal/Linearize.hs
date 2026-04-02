{-# LANGUAGE BangPatterns #-}

module Kernel.Clarify.Internal.Linearize
  ( Handle (..),
    new,
    linearize,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Gensym.Handle qualified as Gensym
import Kernel.Clarify.Internal.Utility qualified as Utility
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.Ident
import Language.Common.Ident.Reify
import Language.Common.LowMagic qualified as LM
import Language.Comp.Comp qualified as C

type Occurrence = Ident

type ActiveSet = IntSet.IntSet

type OccMap = IntMap.IntMap [Occurrence]

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    utilityHandle :: Utility.Handle
  }

new :: Gensym.Handle -> Utility.Handle -> Handle
new gensymHandle utilityHandle = do
  Handle {..}

appendOccMaps :: OccMap -> OccMap -> OccMap
appendOccMaps =
  IntMap.unionWith (++)

linearize ::
  Handle ->
  [(Ident, C.Comp)] -> -- [(x1, t1), ..., (xn, tn)]  (closed chain)
  C.Comp -> -- a term that can contain non-linear occurrences of xi
  IO C.Comp -- a term in which all the variables in the closed chain occur linearly
linearize h binder e = do
  let activeAll = IntSet.fromList $ map (toInt . fst) binder
  (occMap, e') <- distinguishComp h activeAll e
  wrapBinders h activeAll (reverse binder) e' occMap

wrapBinders :: Handle -> ActiveSet -> [(Ident, C.Comp)] -> C.Comp -> OccMap -> IO C.Comp
wrapBinders h activeSet binderEntries body bodyOccMap =
  case binderEntries of
    [] ->
      return body
    (binderName, binderType) : rest -> do
      let activeSet' = IntSet.delete (toInt binderName) activeSet
      case IntMap.findWithDefault [] (toInt binderName) bodyOccMap of
        [] -> do
          hole <- Gensym.newIdentFromText (gensymHandle h) "unit"
          (headerOccMap, t') <- distinguishComp h activeSet' binderType
          discardUnusedVar <- Utility.toAffineApp (utilityHandle h) (C.VarLocal binderName) t'
          let occMap' = appendOccMaps headerOccMap (IntMap.delete (toInt binderName) bodyOccMap)
          let term' = C.UpElim True hole discardUnusedVar body
          wrapBinders h activeSet' rest term' occMap'
        [z] -> do
          let term' = C.UpElim True z (C.UpIntro (C.VarLocal binderName)) body
          wrapBinders h activeSet' rest term' (IntMap.delete (toInt binderName) bodyOccMap)
        z : zs -> do
          localName <- Gensym.newIdentFromText (gensymHandle h) $ toText binderName <> "-local"
          (headerOccMap, headerTerm) <- insertHeader h activeSet' localName z zs binderType body
          let occMap' = appendOccMaps headerOccMap (IntMap.delete (toInt binderName) bodyOccMap)
          let term' = C.UpElim False localName (C.UpIntro (C.VarLocal binderName)) headerTerm
          wrapBinders h activeSet' rest term' occMap'

insertHeader ::
  Handle ->
  ActiveSet ->
  Ident ->
  Occurrence ->
  [Occurrence] ->
  C.Comp ->
  C.Comp ->
  IO (OccMap, C.Comp)
insertHeader h activeSet localName z1 zs t e =
  case zs of
    [] ->
      return (IntMap.empty, C.UpElim True z1 (C.UpIntro (C.VarLocal localName)) e)
    z2 : rest -> do
      (restOccMap, e') <- insertHeader h activeSet localName z2 rest t e
      (copyOccMap, t') <- distinguishComp h activeSet t
      copyRelevantVar <- Utility.toRelevantApp (utilityHandle h) (C.VarLocal localName) t'
      return (appendOccMaps copyOccMap restOccMap, C.UpElim True z1 copyRelevantVar e')

distinguishComp :: Handle -> ActiveSet -> C.Comp -> IO (OccMap, C.Comp)
distinguishComp h activeSet term
  | IntSet.null activeSet =
      return (IntMap.empty, term)
  | otherwise = do
      (occMapRev, term') <- distinguishCompAcc h activeSet term IntMap.empty
      return (fmap reverse occMapRev, term')

distinguishVarAcc :: Handle -> ActiveSet -> Ident -> OccMap -> IO (OccMap, Ident)
distinguishVarAcc h activeSet x !occMap =
  if IntSet.member (toInt x) activeSet
    then do
      x' <- Gensym.newIdentFromIdent (gensymHandle h) x
      let occMap' = IntMap.insertWith (++) (toInt x) [x'] occMap
      return (occMap', x')
    else
      return (occMap, x)

distinguishValueAcc :: Handle -> ActiveSet -> C.Value -> OccMap -> IO (OccMap, C.Value)
distinguishValueAcc h activeSet term !occMap =
  case term of
    C.VarLocal x -> do
      (occMap', x') <- distinguishVarAcc h activeSet x occMap
      return (occMap', C.VarLocal x')
    C.SigmaIntro size ds -> do
      (occMap', ds') <- distinguishValuesAcc h activeSet ds occMap
      return (occMap', C.SigmaIntro size ds')
    _ ->
      return (occMap, term)

distinguishValuesAcc :: Handle -> ActiveSet -> [C.Value] -> OccMap -> IO (OccMap, [C.Value])
distinguishValuesAcc h activeSet ds !occMap =
  case ds of
    [] ->
      return (occMap, [])
    d : rest -> do
      (occMap1, d') <- distinguishValueAcc h activeSet d occMap
      (occMap2, rest') <- distinguishValuesAcc h activeSet rest occMap1
      return (occMap2, d' : rest')

distinguishCompAcc :: Handle -> ActiveSet -> C.Comp -> OccMap -> IO (OccMap, C.Comp)
distinguishCompAcc h activeSet term !occMap =
  case term of
    C.Primitive theta -> do
      (occMap', theta') <- distinguishPrimitiveAcc h activeSet theta occMap
      return (occMap', C.Primitive theta')
    C.PiElimDownElim forceInline d ds -> do
      (occMap1, d') <- distinguishValueAcc h activeSet d occMap
      (occMap2, ds') <- distinguishValuesAcc h activeSet ds occMap1
      return (occMap2, C.PiElimDownElim forceInline d' ds')
    C.SigmaElim shouldDeallocate xs d e -> do
      (occMap1, d') <- distinguishValueAcc h activeSet d occMap
      let activeSet' = removeBoundIdents activeSet xs
      if IntSet.null activeSet'
        then return (occMap1, C.SigmaElim shouldDeallocate xs d' e)
        else do
          (occMap2, e') <- distinguishCompAcc h activeSet' e occMap1
          return (occMap2, C.SigmaElim shouldDeallocate xs d' e')
    C.UpIntro d -> do
      (occMap', d') <- distinguishValueAcc h activeSet d occMap
      return (occMap', C.UpIntro d')
    C.UpIntroVoid ->
      return (occMap, C.UpIntroVoid)
    C.UpElim isReducible x e1 e2 -> do
      (occMap1, e1') <- distinguishCompAcc h activeSet e1 occMap
      let activeSet' = IntSet.delete (toInt x) activeSet
      if IntSet.null activeSet'
        then return (occMap1, C.UpElim isReducible x e1' e2)
        else do
          (occMap2, e2') <- distinguishCompAcc h activeSet' e2 occMap1
          return (occMap2, C.UpElim isReducible x e1' e2')
    C.UpElimCallVoid f ds e2 -> do
      (occMap1, f') <- distinguishValueAcc h activeSet f occMap
      (occMap2, ds') <- distinguishValuesAcc h activeSet ds occMap1
      (occMap3, e2') <- distinguishCompAcc h activeSet e2 occMap2
      return (occMap3, C.UpElimCallVoid f' ds' e2')
    C.EnumElim fvInfo d defaultBranch branchList -> do
      (occMap1, fvInfo') <- distinguishFVInfoAcc h activeSet fvInfo occMap
      (occMap2, d') <- distinguishValueAcc h activeSet d occMap1
      let activeSet' = removeBoundInts activeSet $ map fst fvInfo
      if IntSet.null activeSet'
        then return (occMap2, C.EnumElim fvInfo' d' defaultBranch branchList)
        else do
          (occMap3, defaultBranch') <- distinguishCompAcc h activeSet' defaultBranch occMap2
          (occMap4, branchList') <- distinguishBranchListAcc h activeSet' branchList occMap3
          return (occMap4, C.EnumElim fvInfo' d' defaultBranch' branchList')
    C.DestCall sizeComp f ds -> do
      (occMap1, sizeComp') <- distinguishCompAcc h activeSet sizeComp occMap
      (occMap2, f') <- distinguishValueAcc h activeSet f occMap1
      (occMap3, ds') <- distinguishValuesAcc h activeSet ds occMap2
      return (occMap3, C.DestCall sizeComp' f' ds')
    C.WriteToDest dest sizeComp result cont -> do
      (occMap1, dest') <- distinguishValueAcc h activeSet dest occMap
      (occMap2, sizeComp') <- distinguishCompAcc h activeSet sizeComp occMap1
      (occMap3, result') <- distinguishCompAcc h activeSet result occMap2
      (occMap4, cont') <- distinguishCompAcc h activeSet cont occMap3
      return (occMap4, C.WriteToDest dest' sizeComp' result' cont')
    C.Free x size cont -> do
      (occMap1, x') <- distinguishValueAcc h activeSet x occMap
      (occMap2, cont') <- distinguishCompAcc h activeSet cont occMap1
      return (occMap2, C.Free x' size cont')
    C.Unreachable ->
      return (occMap, term)

distinguishFVInfoAcc :: Handle -> ActiveSet -> [(Int, C.Value)] -> OccMap -> IO (OccMap, [(Int, C.Value)])
distinguishFVInfoAcc h activeSet fvInfo !occMap =
  case fvInfo of
    [] ->
      return (occMap, [])
    (i, d) : rest -> do
      (occMap1, d') <- distinguishValueAcc h activeSet d occMap
      (occMap2, rest') <- distinguishFVInfoAcc h activeSet rest occMap1
      return (occMap2, (i, d') : rest')

distinguishBranchListAcc :: Handle -> ActiveSet -> [(tag, C.Comp)] -> OccMap -> IO (OccMap, [(tag, C.Comp)])
distinguishBranchListAcc h activeSet branchList !occMap =
  case branchList of
    [] ->
      return (occMap, [])
    (tag, branch) : rest -> do
      (occMap1, branch') <- distinguishCompAcc h activeSet branch occMap
      (occMap2, rest') <- distinguishBranchListAcc h activeSet rest occMap1
      return (occMap2, (tag, branch') : rest')

distinguishPrimitiveAcc :: Handle -> ActiveSet -> C.Primitive -> OccMap -> IO (OccMap, C.Primitive)
distinguishPrimitiveAcc h activeSet term !occMap =
  case term of
    C.PrimOp op ds -> do
      (occMap', ds') <- distinguishValuesAcc h activeSet ds occMap
      return (occMap', C.PrimOp op ds')
    C.ShiftPointer v size index -> do
      (occMap', v') <- distinguishValueAcc h activeSet v occMap
      return (occMap', C.ShiftPointer v' size index)
    C.Calloc num size -> do
      (occMap1, num') <- distinguishValueAcc h activeSet num occMap
      (occMap2, size') <- distinguishValueAcc h activeSet size occMap1
      return (occMap2, C.Calloc num' size')
    C.Alloc size -> do
      (occMap', size') <- distinguishValueAcc h activeSet size occMap
      return (occMap', C.Alloc size')
    C.Realloc ptr size -> do
      (occMap1, ptr') <- distinguishValueAcc h activeSet ptr occMap
      (occMap2, size') <- distinguishValueAcc h activeSet size occMap1
      return (occMap2, C.Realloc ptr' size')
    C.Memcpy dest src size -> do
      (occMap1, dest') <- distinguishValueAcc h activeSet dest occMap
      (occMap2, src') <- distinguishValueAcc h activeSet src occMap1
      (occMap3, size') <- distinguishValueAcc h activeSet size occMap2
      return (occMap3, C.Memcpy dest' src' size')
    C.Magic magic ->
      case magic of
        LM.Cast from to value -> do
          (occMap', value') <- distinguishValueAcc h activeSet value occMap
          return (occMap', C.Magic (LM.Cast from to value'))
        LM.Store lt unit value pointer -> do
          (occMap1, value') <- distinguishValueAcc h activeSet value occMap
          (occMap2, pointer') <- distinguishValueAcc h activeSet pointer occMap1
          return (occMap2, C.Magic (LM.Store lt unit value' pointer'))
        LM.Load lt pointer -> do
          (occMap', pointer') <- distinguishValueAcc h activeSet pointer occMap
          return (occMap', C.Magic (LM.Load lt pointer'))
        LM.Alloca lt num ->
          return (occMap, C.Magic (LM.Alloca lt num))
        LM.External domList cod extFunName args varArgAndTypeList -> do
          (occMap1, args') <- distinguishValuesAcc h activeSet args occMap
          (occMap2, varArgAndTypeList') <- distinguishVarArgListAcc h activeSet varArgAndTypeList occMap1
          return (occMap2, C.Magic (LM.External domList cod extFunName args' varArgAndTypeList'))
        LM.Global name lt ->
          return (occMap, C.Magic (LM.Global name lt))
        LM.OpaqueValue e -> do
          (occMap', e') <- distinguishValueAcc h activeSet e occMap
          return (occMap', C.Magic (LM.OpaqueValue e'))
        LM.CallType func arg1 arg2 -> do
          (occMap1, arg1') <- distinguishValueAcc h activeSet arg1 occMap
          (occMap2, arg2') <- distinguishValueAcc h activeSet arg2 occMap1
          return (occMap2, C.Magic (LM.CallType func arg1' arg2'))

distinguishVarArgListAcc :: Handle -> ActiveSet -> [(C.Value, t)] -> OccMap -> IO (OccMap, [(C.Value, t)])
distinguishVarArgListAcc h activeSet varArgAndTypeList !occMap =
  case varArgAndTypeList of
    [] ->
      return (occMap, [])
    (varArg, varType) : rest -> do
      (occMap1, varArg') <- distinguishValueAcc h activeSet varArg occMap
      (occMap2, rest') <- distinguishVarArgListAcc h activeSet rest occMap1
      return (occMap2, (varArg', varType) : rest')

removeBoundIdents :: ActiveSet -> [Ident] -> ActiveSet
removeBoundIdents =
  foldl (flip $ IntSet.delete . toInt)

removeBoundInts :: ActiveSet -> [Int] -> ActiveSet
removeBoundInts =
  foldl (flip IntSet.delete)
