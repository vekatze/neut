module Kernel.Clarify.Move.Internal.Sigma
  ( Handle (..),
    new,
    registerImmediateS4,
    registerClosureS4,
    immediateNullS4,
    returnImmediateTypeS4,
    returnImmediateEnumS4,
    returnImmediateNoemaS4,
    returnImmediateIntS4,
    returnImmediateFloatS4,
    returnImmediateRuneS4,
    returnImmediatePointerS4,
    returnImmediateNullS4,
    returnClosureS4,
    closureEnvS4,
    returnSigmaDataS4,
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
  registerSigmaS4
    h
    DD.cls
    O.Clear
    [Right (env, returnImmediateTypeS4), Left (C.UpIntro envVar), Left returnImmediatePointerS4]
    (newTagMaker Function)

returnImmediateTypeS4 :: C.Comp
returnImmediateTypeS4 = do
  C.UpIntro (C.VarGlobal DD.immType AN.argNumS4)

returnImmediateEnumS4 :: C.Comp
returnImmediateEnumS4 = do
  C.UpIntro (C.VarGlobal DD.immEnum AN.argNumS4)

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
  [Either C.Comp (Ident, C.Comp)] ->
  C.Comp ->
  IO ()
registerSigmaS4 h name opacity mxts tagMaker = do
  resourceSpec <- makeSigmaResourceSpec h mxts tagMaker
  Utility.registerSwitcher (utilityHandle h) opacity name resourceSpec

makeSigmaResourceSpec :: Handle -> [Either C.Comp (Ident, C.Comp)] -> C.Comp -> IO ResourceSpec
makeSigmaResourceSpec h mxts tagMaker = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  discard <- sigmaT h mxts argVar
  copy <- sigma4 h mxts argVar
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
  [Either C.Comp (Ident, C.Comp)] ->
  C.Value ->
  IO C.Comp
sigmaT h mxts argVar = do
  xts <- liftIO $ mapM (supplyName (gensymHandle h)) mxts
  -- as == [APP-1, ..., APP-n]   (`a` here stands for `app`)
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
  [Either C.Comp (Ident, C.Comp)] ->
  C.Value ->
  IO C.Comp
sigma4 h mxts argVar = do
  xts <- liftIO $ mapM (supplyName (gensymHandle h)) mxts
  -- as == [APP-1, ..., APP-n]
  as <- forM xts $ \(x, t) -> do
    Utility.toRelevantApp (utilityHandle h) (C.VarLocal x) t
  (varNameList, varList) <- mapAndUnzipM (const $ Gensym.createVar (gensymHandle h) "pair") xts
  body' <- Linearize.linearize (linearizeHandle h) xts $ Utility.bindLet (zip varNameList as) $ C.UpIntro $ C.SigmaIntro varList
  return $ C.SigmaElim False (map fst xts) argVar body'

supplyName :: Gensym.Handle -> Either b (Ident, b) -> IO (Ident, b)
supplyName h mName =
  case mName of
    Right (x, t) ->
      return (x, t)
    Left t -> do
      x <- Gensym.newIdentFromText h "unused-sigarg"
      return (x, t)

closureEnvS4 ::
  Handle ->
  Locator.Handle ->
  [Either C.Comp (Ident, C.Comp)] ->
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
  [(D.Discriminant, [(Ident, C.Comp)])] ->
  IO C.Comp
returnSigmaDataS4 h dataName opacity dataInfo = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  discard <- sigmaDataT h dataInfo argVar
  copy <- sigmaData4 h dataInfo argVar
  let dataName' = DD.getFormDD dataName
  Utility.registerSwitcher (utilityHandle h) opacity dataName' $ ResourceSpec {switch, arg, defaultClause = newTagMaker Algebraic, clauses = [discard, copy]}
  return $ C.UpIntro $ C.VarGlobal dataName' AN.argNumS4

sigmaData4 :: Handle -> [(D.Discriminant, [(Ident, C.Comp)])] -> C.Value -> IO C.Comp
sigmaData4 h = do
  sigmaData h (sigmaBinder4 h)

sigmaBinder4 :: Handle -> [(Ident, C.Comp)] -> C.Value -> IO C.Comp
sigmaBinder4 h xts v = do
  sigma4 h (Left (returnImmediateIntS4 IntSize64) : map Right xts) v

sigmaDataT :: Handle -> [(D.Discriminant, [(Ident, C.Comp)])] -> C.Value -> IO C.Comp
sigmaDataT h = do
  sigmaData h (sigmaBinderT h)

sigmaData ::
  Handle ->
  ([(Ident, C.Comp)] -> C.Value -> IO C.Comp) ->
  [(D.Discriminant, [(Ident, C.Comp)])] ->
  C.Value ->
  IO C.Comp
sigmaData h resourceHandler dataInfo arg = do
  case dataInfo of
    [] ->
      return $ C.UpIntro arg
    _ -> do
      let (discList, binderList) = unzip dataInfo
      let discList' = map discriminantToEnumCase discList
      localName <- Gensym.newIdentFromText (gensymHandle h) "local"
      binderList' <- mapM (`resourceHandler` C.VarLocal localName) binderList
      (disc, discVar) <- Gensym.createVar (gensymHandle h) "disc"
      enumElim <- Utility.getEnumElim (utilityHandle h) [localName] discVar (last binderList') (zip discList' (init binderList'))
      return $
        C.UpElim False localName (C.UpIntro arg) $
          C.UpElim True disc (C.Primitive (C.Magic (M.Load BLT.Pointer (C.VarLocal localName)))) enumElim

sigmaBinderT :: Handle -> [(Ident, C.Comp)] -> C.Value -> IO C.Comp
sigmaBinderT h xts v = do
  sigmaT h (Left (returnImmediateIntS4 IntSize64) : map Right xts) v

discriminantToEnumCase :: D.Discriminant -> EC.EnumCase
discriminantToEnumCase discriminant =
  EC.Int (D.reify discriminant)

newTagMaker :: TypeTag -> C.Comp
newTagMaker tag = do
  C.UpIntro $
    C.Int IntSize64 (typeTagToInteger tag)
