module Kernel.Clarify.Internal.Sigma
  ( Handle (..),
    DataConstructorInfo (..),
    new,
    registerImmediateS4,
    registerClosureS4,
    immediateNullS4,
    returnImmediateTypeS4,
    returnImmediateNoemaS4,
    returnImmediateIntS4,
    returnImmediateFloatS4,
    returnImmediateRuneS4,
    returnImmediatePointerS4,
    returnImmediateNullS4,
    returnClosureS4,
    closureEnvS4,
    returnSigmaDataS4,
    returnSigmaEnumS4,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
import Kernel.Clarify.Internal.Linearize qualified as Linearize
import Kernel.Clarify.Internal.Utility (ResourceSpec (ResourceSpec))
import Kernel.Clarify.Internal.Utility qualified as Utility
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.TypeTag
import Language.Common.ArgNum qualified as AN
import Language.Common.BaseLowType qualified as BLT
import Language.Common.BaseName qualified as BN
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.Ident
import Language.Common.Magic qualified as M
import Language.Common.Opacity qualified as O
import Language.Common.PrimNumSize (FloatSize (..), IntSize (..))
import Language.Comp.Comp qualified as C
import Language.Comp.CreateVar qualified as Gensym
import Language.Comp.EnumCase qualified as EC

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    linearizeHandle :: Linearize.Handle,
    utilityHandle :: Utility.Handle
  }

data DataConstructorInfo = DataConstructorInfo
  { consName :: DD.DefiniteDescription,
    isConstLike :: Bool,
    discriminant :: D.Discriminant,
    dataArgs :: [(Ident, C.Comp)],
    consArgs :: [(Ident, C.Comp)]
  }

new :: Gensym.Handle -> Linearize.Handle -> Utility.Handle -> Handle
new gensymHandle linearizeHandle utilityHandle = do
  Handle {..}

registerImmediateS4 :: Handle -> IO ()
registerImmediateS4 h = do
  forM_ immTypeTagMap $ \(name, typeTag) -> do
    switch <- Gensym.createVar (gensymHandle h) "switch"
    arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
    let discard = C.UpIntro $ C.SigmaIntro []
    let copy = C.UpIntro argVar
    Utility.registerSwitcher (utilityHandle h) O.Clear name $ do
      ResourceSpec {switch, arg, defaultClause = newTagMaker typeTag, clauses = [discard, copy]}

registerClosureS4 :: Handle -> IO ()
registerClosureS4 h = do
  (env, envVar) <- Gensym.createVar (gensymHandle h) "env"
  hole1 <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
  hole2 <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
  registerSigmaS4
    h
    DD.cls
    O.Clear
    [(env, returnImmediateTypeS4), (hole1, C.UpIntro envVar), (hole2, returnImmediatePointerS4)]
    (newTagMaker Function)

returnImmediateTypeS4 :: C.Comp
returnImmediateTypeS4 = do
  C.UpIntro (C.VarGlobal DD.immType AN.argNumS4)

returnImmediateNoemaS4 :: C.Comp
returnImmediateNoemaS4 = do
  C.UpIntro (C.VarGlobal DD.immNoema AN.argNumS4)

returnImmediateIntS4 :: IntSize -> C.Comp
returnImmediateIntS4 intSize = do
  case intSize of
    IntSize1 ->
      C.UpIntro (C.VarGlobal DD.immInt1 AN.argNumS4)
    IntSize2 ->
      C.UpIntro (C.VarGlobal DD.immInt2 AN.argNumS4)
    IntSize4 ->
      C.UpIntro (C.VarGlobal DD.immInt4 AN.argNumS4)
    IntSize8 ->
      C.UpIntro (C.VarGlobal DD.immInt8 AN.argNumS4)
    IntSize16 ->
      C.UpIntro (C.VarGlobal DD.immInt16 AN.argNumS4)
    IntSize32 ->
      C.UpIntro (C.VarGlobal DD.immInt32 AN.argNumS4)
    IntSize64 ->
      C.UpIntro (C.VarGlobal DD.immInt64 AN.argNumS4)

returnImmediateFloatS4 :: FloatSize -> C.Comp
returnImmediateFloatS4 floatSize = do
  case floatSize of
    FloatSize16 ->
      C.UpIntro (C.VarGlobal DD.immFloat16 AN.argNumS4)
    FloatSize32 ->
      C.UpIntro (C.VarGlobal DD.immFloat32 AN.argNumS4)
    FloatSize64 ->
      C.UpIntro (C.VarGlobal DD.immFloat64 AN.argNumS4)

returnImmediateRuneS4 :: C.Comp
returnImmediateRuneS4 = do
  C.UpIntro (C.VarGlobal DD.immRune AN.argNumS4)

returnImmediatePointerS4 :: C.Comp
returnImmediatePointerS4 = do
  C.UpIntro (C.VarGlobal DD.immPointer AN.argNumS4)

returnImmediateNullS4 :: C.Comp
returnImmediateNullS4 = do
  C.UpIntro immediateNullS4

returnClosureS4 :: C.Comp
returnClosureS4 = do
  C.UpIntro $ C.VarGlobal DD.cls AN.argNumS4

immediateNullS4 :: C.Value
immediateNullS4 = do
  C.VarGlobal DD.immNull AN.argNumS4

registerSigmaS4 ::
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  [(Ident, C.Comp)] ->
  C.Comp ->
  IO ()
registerSigmaS4 h name opacity xts tagMaker = do
  resourceSpec <- makeSigmaResourceSpec h xts tagMaker
  Utility.registerSwitcher (utilityHandle h) opacity name resourceSpec

makeSigmaResourceSpec :: Handle -> [(Ident, C.Comp)] -> C.Comp -> IO ResourceSpec
makeSigmaResourceSpec h xts tagMaker = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  discard <- sigmaT h xts argVar
  copy <- sigma4 h xts argVar
  return $ ResourceSpec {switch, arg, defaultClause = tagMaker, clauses = [discard, copy]}

-- (Assuming `ti` = `return di` for some `di` such that `xi : di`)
-- sigmaT NAME LOC [(x1, t1), ..., (xn, tn)]   ~>
--   update CompEnv with NAME ~> (thunk LAM), where LAM is:
--   lam z.
--     let (x1, ..., xn) := z in
--     <LINEARIZE_HEADER for x1, .., xn> in                     ---
--     bind y1 :=                                    ---        ---
--       bind f1 = t1 in              ---            ---        ---
--       f1 @ (0, x1) in              ---  APP-1     ---        ---
--     ...                                           ---  body  ---  body'
--     bind yn :=                                    ---        ---
--       bind fn = tn in              ---            ---        ---
--       fn @ (0, xn) in              ---  APP-n     ---        ---
--     return ()                                     ---        ---
--
sigmaT ::
  Handle ->
  [(Ident, C.Comp)] ->
  C.Value ->
  IO C.Comp
sigmaT h xts argVar = do
  as <- forM xts $ \(x, t) -> do
    Utility.toAffineApp (utilityHandle h) (C.VarLocal x) t
  ys <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "arg") xts
  body' <- Linearize.linearize (linearizeHandle h) xts $ Utility.bindLet (zip ys as) $ C.UpIntro $ C.SigmaIntro []
  return $ C.SigmaElim True (map fst xts) argVar body'

-- (Assuming `ti` = `return di` for some `di` such that `xi : di`)
-- sigma4 NAME LOC [(x1, t1), ..., (xn, tn)]   ~>
--   update CompEnv with NAME ~> (thunk LAM), where LAM is:
--   lam z.
--     let-without-free (x1, ..., xn) := z in
--     <LINEARIZE_HEADER for x1, .., xn> in                                      ---
--     bind x1' :=                                                     ---       ---
--       bind f1 = t1 in              ---                              ---       ---
--       f1 @ (1, x1) in              ---  APP-1                       ---       ---
--     ...                                                             ---       ---
--     bind xn' :=                                                     --- body  --- body'
--       bind fn = tn in              ---                              ---       ---
--       fn @ (1, xn) in              ---  APP-n                       ---       ---
--     return (x1', ..., xn')
sigma4 ::
  Handle ->
  [(Ident, C.Comp)] ->
  C.Value ->
  IO C.Comp
sigma4 h xts argVar = do
  -- as == [APP-1, ..., APP-n]
  as <- forM xts $ \(x, t) -> do
    Utility.toRelevantApp (utilityHandle h) (C.VarLocal x) t
  (varNameList, varList) <- mapAndUnzipM (const $ Gensym.createVar (gensymHandle h) "pair") xts
  body' <- Linearize.linearize (linearizeHandle h) xts $ Utility.bindLet (zip varNameList as) $ C.UpIntro $ C.SigmaIntro varList
  return $ C.SigmaElim False (map fst xts) argVar body'

storeAtOffset ::
  Handle ->
  C.Value -> -- source array
  Integer -> -- total size
  Integer -> -- offset index
  C.Comp -> -- type computation
  IO C.Comp
storeAtOffset h argVar size index comp = do
  (pointerVarName, pointerVar) <- Gensym.createVar (gensymHandle h) "pointer"
  (tmpVarName, tmpVar) <- Gensym.createVar (gensymHandle h) "tmp"
  return $
    C.UpElim True pointerVarName (C.Primitive $ C.ShiftPointer argVar size index) $
      C.UpElim True tmpVarName comp $
        C.Primitive $
          C.Magic $
            M.Store BLT.Pointer (C.SigmaIntro []) tmpVar pointerVar

-- lam z.
--   let-without-free (x1, ..., xn) := z;
--   <∀i. copy xi if xi is used multiple times>;
--   z[0] = t0;
--   z[1] = t1;
--   ...
--   z[n] = tn;
--   return unit
sigmaStoreTypeClause ::
  Handle ->
  DataConstructorInfo ->
  C.Value ->
  IO C.Comp
sigmaStoreTypeClause h info argVar = do
  x <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
  let xts = (x, returnImmediateIntS4 IntSize64) : dataArgs info ++ consArgs info
  let size = toInteger $ length xts + 2
  a0 <- storeAtOffset h argVar size 0 (C.UpIntro $ C.VarStaticText $ DD.localLocator (consName info))
  as <- forM (zip [1 .. size - 3] (drop 1 xts)) $ \(index, (_, t)) ->
    storeAtOffset h argVar size index t
  let dataArgLen = toInteger $ length $ dataArgs info
  an <- storeAtOffset h argVar size (size - 2) (C.UpIntro $ C.Int IntSize64 dataArgLen)
  let isConstLike' = if isConstLike info then 1 else 0
  an2 <- storeAtOffset h argVar size (size - 1) (C.UpIntro $ C.Int IntSize64 isConstLike')
  ys <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "arg") [1 .. length xts + 2]
  body' <-
    Linearize.linearizeDuplicatedVariables (linearizeHandle h) xts $
      Utility.bindLet (zip ys (a0 : as ++ [an, an2])) $
        C.UpIntro $
          C.SigmaIntro []
  return $ C.SigmaElim False (map fst xts) argVar body'

sigmaEnumConsName ::
  Handle ->
  [(DD.DefiniteDescription, D.Discriminant)] ->
  C.Value ->
  IO C.Comp
sigmaEnumConsName h enumInfo arg = do
  let discList' = map (discriminantToEnumCase . snd) enumInfo
  let branchList = map (\(name, _) -> C.UpIntro $ C.VarStaticText $ DD.localLocator name) enumInfo
  let defaultBranch = C.UpIntro (C.SigmaIntro [])
  (disc, discVar) <- Gensym.createVar (gensymHandle h) "disc"
  enumElim <- Utility.getEnumElim (utilityHandle h) [] discVar defaultBranch (zip discList' branchList)
  return $ C.UpElim True disc (C.UpIntro arg) enumElim

closureEnvS4 ::
  Handle ->
  Locator.Handle ->
  [(Ident, C.Comp)] ->
  IO C.Value
closureEnvS4 h locatorHandle mxts =
  case mxts of
    [] ->
      return immediateNullS4 -- performance optimization; not necessary for correctness
    _ -> do
      i <- Gensym.newCount (gensymHandle h)
      let name = Locator.attachCurrentLocator locatorHandle $ BN.sigmaName i
      resourceSpec <- makeSigmaResourceSpec h mxts (newTagMaker Opaque)
      liftIO $ Utility.registerSwitcher (utilityHandle h) O.Clear name resourceSpec
      return $ C.VarGlobal name AN.argNumS4

returnSigmaDataS4 ::
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  [DataConstructorInfo] ->
  IO C.Comp
returnSigmaDataS4 h dataName opacity dataInfo = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  discard <- sigmaDataT h dataInfo argVar
  copy <- sigmaData4 h dataInfo argVar
  consSizeClause <- sigmaDataConsSize h dataInfo argVar
  let dataName' = DD.getFormDD dataName
  let clauses = [discard, copy, newTagMaker Algebraic, consSizeClause]
  defaultClause <- sigmaStoreType h dataInfo argVar
  Utility.registerSwitcher (utilityHandle h) opacity dataName' $
    ResourceSpec {switch, arg, defaultClause, clauses}
  return $ C.UpIntro $ C.VarGlobal dataName' AN.argNumS4

returnSigmaEnumS4 ::
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  [(DD.DefiniteDescription, Bool, D.Discriminant)] ->
  IO C.Comp
returnSigmaEnumS4 h dataName opacity enumInfo = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  let discard = C.UpIntro $ C.SigmaIntro []
  let copy = C.UpIntro argVar
  consSizeClause <- sigmaEnumConsSize h (map (\(_, _, d) -> d) enumInfo) argVar
  defaultClause <- sigmaEnumConsName h (map (\(name, _, d) -> (name, d)) enumInfo) argVar
  let dataName' = DD.getFormDD dataName
  let clauses = [discard, copy, newTagMaker Enum, consSizeClause]
  Utility.registerSwitcher (utilityHandle h) opacity dataName' $
    ResourceSpec {switch, arg, defaultClause, clauses}
  return $ C.UpIntro $ C.VarGlobal dataName' AN.argNumS4

sigmaData4 :: Handle -> [DataConstructorInfo] -> C.Value -> IO C.Comp
sigmaData4 h = do
  sigmaData h (sigmaBinder4 h)

sigmaDataT :: Handle -> [DataConstructorInfo] -> C.Value -> IO C.Comp
sigmaDataT h = do
  sigmaData h (sigmaBinderT h)

sigmaStoreType :: Handle -> [DataConstructorInfo] -> C.Value -> IO C.Comp
sigmaStoreType h = do
  sigmaData h (sigmaStoreTypeClause h)

sigmaBinder4 :: Handle -> DataConstructorInfo -> C.Value -> IO C.Comp
sigmaBinder4 h info v = do
  let xts = dataArgs info ++ consArgs info
  x <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
  sigma4 h ((x, returnImmediateIntS4 IntSize64) : xts) v

sigmaBinderT :: Handle -> DataConstructorInfo -> C.Value -> IO C.Comp
sigmaBinderT h info v = do
  let xts = dataArgs info ++ consArgs info
  x <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
  sigmaT h ((x, returnImmediateIntS4 IntSize64) : xts) v

sigmaData ::
  Handle ->
  (DataConstructorInfo -> C.Value -> IO C.Comp) ->
  [DataConstructorInfo] ->
  C.Value ->
  IO C.Comp
sigmaData h resourceHandler dataInfo arg = do
  case dataInfo of
    [] ->
      return $ C.UpIntro arg
    _ -> do
      let discList' = map (discriminantToEnumCase . discriminant) dataInfo
      localName <- Gensym.newIdentFromText (gensymHandle h) "local"
      binderList' <- mapM (\info -> resourceHandler info (C.VarLocal localName)) dataInfo
      (disc, discVar) <- Gensym.createVar (gensymHandle h) "disc"
      enumElim <- Utility.getEnumElim (utilityHandle h) [localName] discVar (last binderList') (zip discList' (init binderList'))
      return $
        C.UpElim False localName (C.UpIntro arg) $
          C.UpElim True disc (C.Primitive (C.Magic (M.Load BLT.Pointer (C.VarLocal localName)))) enumElim

sigmaDataConsSize ::
  Handle ->
  [DataConstructorInfo] ->
  C.Value ->
  IO C.Comp
sigmaDataConsSize h dataInfo arg = do
  let discList' = map (discriminantToEnumCase . discriminant) dataInfo
  let branchList = flip map dataInfo $ \info -> do
        let numOfArgs = toInteger $ length (dataArgs info ++ consArgs info)
        C.UpIntro $ C.Int IntSize64 (numOfArgs + 1) -- `+1` is for discriminants
  let defaultBranch = C.UpIntro (C.Int IntSize64 (-1))
  (disc, discVar) <- Gensym.createVar (gensymHandle h) "disc"
  enumElim <- Utility.getEnumElim (utilityHandle h) [] discVar defaultBranch (zip discList' branchList)
  return $ C.UpElim True disc (C.UpIntro arg) enumElim

sigmaEnumConsSize ::
  Handle ->
  [D.Discriminant] ->
  C.Value ->
  IO C.Comp
sigmaEnumConsSize h discList arg = do
  let discList' = map discriminantToEnumCase discList
  let branchList = map (const $ C.UpIntro $ C.Int IntSize64 0) discList
  let defaultBranch = C.UpIntro (C.Int IntSize64 (-1))
  (disc, discVar) <- Gensym.createVar (gensymHandle h) "disc"
  enumElim <- Utility.getEnumElim (utilityHandle h) [] discVar defaultBranch (zip discList' branchList)
  return $ C.UpElim True disc (C.UpIntro arg) enumElim

discriminantToEnumCase :: D.Discriminant -> EC.EnumCase
discriminantToEnumCase discriminant =
  EC.Int (D.reify discriminant)

newTagMaker :: TypeTag -> C.Comp
newTagMaker tag = do
  C.UpIntro $
    C.Int IntSize64 (typeTagToInteger tag)
