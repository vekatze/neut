module Kernel.Clarify.Move.Internal.Sigma
  ( Handle (..),
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
import Gensym.Move.Gensym qualified as Gensym
import Gensym.Rule.Handle qualified as Gensym
import Kernel.Clarify.Move.Internal.Linearize qualified as Linearize
import Kernel.Clarify.Move.Internal.Utility (ResourceSpec (ResourceSpec))
import Kernel.Clarify.Move.Internal.Utility qualified as Utility
import Kernel.Common.Move.Handle.Local.Locator qualified as Locator
import Kernel.Common.Rule.Handle.Local.Locator qualified as Locator
import Kernel.Common.Rule.TypeTag
import Language.Common.Move.CreateSymbol qualified as Gensym
import Language.Common.Rule.ArgNum qualified as AN
import Language.Common.Rule.BaseLowType qualified as BLT
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Discriminant qualified as D
import Language.Common.Rule.Ident
import Language.Common.Rule.Magic qualified as M
import Language.Common.Rule.Opacity qualified as O
import Language.Common.Rule.PrimNumSize (FloatSize (..), IntSize (..))
import Language.Comp.Move.CreateVar qualified as Gensym
import Language.Comp.Rule.Comp qualified as C
import Language.Comp.Rule.EnumCase qualified as EC

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    linearizeHandle :: Linearize.Handle,
    utilityHandle :: Utility.Handle
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
  DD.DefiniteDescription ->
  [(Ident, C.Comp)] ->
  C.Value ->
  IO C.Comp
sigmaStoreTypeClause h consName xts argVar = do
  let consNameText = C.UpIntro $ C.VarStaticText $ DD.localLocator consName
  a0 <- do
    (textVarName, textVar) <- Gensym.createVar (gensymHandle h) "text"
    return $
      C.UpElim True textVarName consNameText $
        C.Primitive $
          C.Magic $
            M.Store BLT.Pointer (C.SigmaIntro []) textVar argVar
  let size = toInteger $ length xts
  as <- forM (zip [1 ..] (drop 1 xts)) $ \(index, (_, t)) -> do
    (pointerVarName, pointerVar) <- Gensym.createVar (gensymHandle h) "pointer"
    (typeVarName, typeVar) <- Gensym.createVar (gensymHandle h) "type"
    return $
      C.UpElim True pointerVarName (C.Primitive $ C.ShiftPointer argVar size index) $
        C.UpElim True typeVarName t $
          C.Primitive $
            C.Magic $
              M.Store BLT.Pointer (C.SigmaIntro []) typeVar pointerVar
  ys <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "arg") xts
  body' <-
    Linearize.linearizeDuplicatedVariables (linearizeHandle h) xts $
      Utility.bindLet (zip ys (a0 : as)) $
        C.UpIntro $
          C.SigmaIntro []
  return $ C.SigmaElim False (map fst xts) argVar body'

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
  [(DD.DefiniteDescription, D.Discriminant, [(Ident, C.Comp)])] ->
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
  [D.Discriminant] ->
  IO C.Comp
returnSigmaEnumS4 h dataName opacity discriminantList = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  let discard = C.UpIntro $ C.SigmaIntro []
  let copy = C.UpIntro argVar
  consSizeClause <- sigmaEnumConsSize h discriminantList argVar
  let dataName' = DD.getFormDD dataName
  Utility.registerSwitcher (utilityHandle h) opacity dataName' $
    ResourceSpec {switch, arg, defaultClause = consSizeClause, clauses = [discard, copy, newTagMaker Enum]}
  return $ C.UpIntro $ C.VarGlobal dataName' AN.argNumS4

sigmaData4 :: Handle -> [(DD.DefiniteDescription, D.Discriminant, [(Ident, C.Comp)])] -> C.Value -> IO C.Comp
sigmaData4 h = do
  sigmaData h (sigmaBinder4 h)

sigmaDataT :: Handle -> [(DD.DefiniteDescription, D.Discriminant, [(Ident, C.Comp)])] -> C.Value -> IO C.Comp
sigmaDataT h = do
  sigmaData h (sigmaBinderT h)

sigmaStoreType :: Handle -> [(DD.DefiniteDescription, D.Discriminant, [(Ident, C.Comp)])] -> C.Value -> IO C.Comp
sigmaStoreType h = do
  sigmaData h $ \consName xts v -> do
    x <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
    sigmaStoreTypeClause h consName ((x, returnImmediateIntS4 IntSize64) : xts) v

sigmaBinder4 :: Handle -> DD.DefiniteDescription -> [(Ident, C.Comp)] -> C.Value -> IO C.Comp
sigmaBinder4 h _consName xts v = do
  x <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
  sigma4 h ((x, returnImmediateIntS4 IntSize64) : xts) v

sigmaBinderT :: Handle -> DD.DefiniteDescription -> [(Ident, C.Comp)] -> C.Value -> IO C.Comp
sigmaBinderT h _consName xts v = do
  x <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
  sigmaT h ((x, returnImmediateIntS4 IntSize64) : xts) v

sigmaData ::
  Handle ->
  (DD.DefiniteDescription -> [(Ident, C.Comp)] -> C.Value -> IO C.Comp) ->
  [(DD.DefiniteDescription, D.Discriminant, [(Ident, C.Comp)])] ->
  C.Value ->
  IO C.Comp
sigmaData h resourceHandler dataInfo arg = do
  case dataInfo of
    [] ->
      return $ C.UpIntro arg
    _ -> do
      let (consNameList, discList, binderList) = unzip3 dataInfo
      let discList' = map discriminantToEnumCase discList
      localName <- Gensym.newIdentFromText (gensymHandle h) "local"
      binderList' <- zipWithM (\consName binder -> resourceHandler consName binder (C.VarLocal localName)) consNameList binderList
      (disc, discVar) <- Gensym.createVar (gensymHandle h) "disc"
      enumElim <- Utility.getEnumElim (utilityHandle h) [localName] discVar (last binderList') (zip discList' (init binderList'))
      return $
        C.UpElim False localName (C.UpIntro arg) $
          C.UpElim True disc (C.Primitive (C.Magic (M.Load BLT.Pointer (C.VarLocal localName)))) enumElim

sigmaDataConsSize ::
  Handle ->
  [(DD.DefiniteDescription, D.Discriminant, [(Ident, C.Comp)])] ->
  C.Value ->
  IO C.Comp
sigmaDataConsSize h dataInfo arg = do
  let discList' = map (discriminantToEnumCase . (\(_, disc, _) -> disc)) dataInfo
  let branchList = flip map dataInfo $ \(_, _, consArgs) -> do
        C.UpIntro $ C.Int IntSize64 (toInteger $ length consArgs + 1) -- `+1` is for discriminants
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
