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
import Data.Text qualified as T
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
import Kernel.Clarify.Internal.Linearize qualified as Linearize
import Kernel.Clarify.Internal.Utility (ResourceSpec (..))
import Kernel.Clarify.Internal.Utility qualified as Utility
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Language.Common.ArgNum qualified as AN
import Language.Common.BaseLowType qualified as BLT
import Language.Common.BaseName qualified as BN
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident
import Language.Common.LowMagic qualified as LM
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
    consArgs :: [(Ident, C.Comp)],
    totalSlotCount :: Int
  }

new :: Gensym.Handle -> Linearize.Handle -> Utility.Handle -> Handle
new gensymHandle linearizeHandle utilityHandle = do
  Handle {..}

globalPointer :: DD.DefiniteDescription -> AN.ArgNum -> C.Value
globalPointer name argNum =
  C.VarGlobal name argNum (FCT.Cod BLT.Pointer)

registerImmediateS4 :: Handle -> IO ()
registerImmediateS4 h = do
  forM_ DD.immTypes $ \name -> do
    switch <- Gensym.createVar (gensymHandle h) "switch"
    arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
    let discard = C.UpIntro $ C.SigmaIntro []
    let copy = C.UpIntro argVar
    Utility.registerSwitcher (utilityHandle h) O.Clear name $ do
      ResourceSpec {switch, arg, discard, copy, size = Utility.returnIntComp (utilityHandle h) (-1), defaultValues = []}

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

returnImmediateTypeS4 :: C.Comp
returnImmediateTypeS4 = do
  C.UpIntro (globalPointer DD.immType AN.argNumS4)

returnImmediateNoemaS4 :: C.Comp
returnImmediateNoemaS4 = do
  C.UpIntro (globalPointer DD.immNoema AN.argNumS4)

returnImmediateIntS4 :: IntSize -> C.Comp
returnImmediateIntS4 intSize = do
  case intSize of
    IntSize1 ->
      C.UpIntro (globalPointer DD.immInt1 AN.argNumS4)
    IntSize2 ->
      C.UpIntro (globalPointer DD.immInt2 AN.argNumS4)
    IntSize4 ->
      C.UpIntro (globalPointer DD.immInt4 AN.argNumS4)
    IntSize8 ->
      C.UpIntro (globalPointer DD.immInt8 AN.argNumS4)
    IntSize16 ->
      C.UpIntro (globalPointer DD.immInt16 AN.argNumS4)
    IntSize32 ->
      C.UpIntro (globalPointer DD.immInt32 AN.argNumS4)
    IntSize64 ->
      C.UpIntro (globalPointer DD.immInt64 AN.argNumS4)

returnImmediateFloatS4 :: FloatSize -> C.Comp
returnImmediateFloatS4 floatSize = do
  case floatSize of
    FloatSize16 ->
      C.UpIntro (globalPointer DD.immFloat16 AN.argNumS4)
    FloatSize32 ->
      C.UpIntro (globalPointer DD.immFloat32 AN.argNumS4)
    FloatSize64 ->
      C.UpIntro (globalPointer DD.immFloat64 AN.argNumS4)

returnImmediateRuneS4 :: C.Comp
returnImmediateRuneS4 = do
  C.UpIntro (globalPointer DD.immRune AN.argNumS4)

returnImmediatePointerS4 :: C.Comp
returnImmediatePointerS4 = do
  C.UpIntro (globalPointer DD.immPointer AN.argNumS4)

returnImmediateNullS4 :: C.Comp
returnImmediateNullS4 = do
  C.UpIntro immediateNullS4

returnClosureS4 :: C.Comp
returnClosureS4 = do
  C.UpIntro $ globalPointer DD.cls AN.argNumS4

immediateNullS4 :: C.Value
immediateNullS4 = do
  globalPointer DD.immNull AN.argNumS4

registerSigmaS4 ::
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  [(Ident, C.Comp)] ->
  IO ()
registerSigmaS4 h name opacity xts = do
  resourceSpec <- makeSigmaResourceSpec h xts
  Utility.registerSwitcher (utilityHandle h) opacity name resourceSpec

makeSigmaResourceSpec :: Handle -> [(Ident, C.Comp)] -> IO ResourceSpec
makeSigmaResourceSpec h xts = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  discard <- sigmaT h xts argVar
  copy <- sigma4 h xts argVar
  return $ ResourceSpec {switch, arg, discard, copy, size = Utility.returnIntComp (utilityHandle h) (toInteger $ length xts), defaultValues = []}

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

closureEnvS4 ::
  Handle ->
  Maybe T.Text ->
  Locator.Handle ->
  [(Ident, C.Comp)] ->
  [C.Value] ->
  IO C.Value
closureEnvS4 h mName locatorHandle mxts defaultValues =
  case mxts of
    []
      | null defaultValues ->
          return immediateNullS4 -- performance optimization; not necessary for correctness
    _ -> do
      i <- Gensym.newCount (gensymHandle h)
      let name = Locator.attachCurrentLocator locatorHandle $ BN.sigmaName mName i
      resourceSpec <- makeSigmaResourceSpec h mxts
      let resourceSpec' = resourceSpec {defaultValues}
      liftIO $ Utility.registerSwitcher (utilityHandle h) O.Clear name resourceSpec'
      return $ globalPointer name AN.argNumS4

returnSigmaDataS4 ::
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  Int ->
  [DataConstructorInfo] ->
  IO C.Comp
returnSigmaDataS4 h dataName opacity totalSlotCount dataInfo = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  discard <- sigmaDataT h dataInfo argVar
  copy <- sigmaData4 h dataInfo argVar
  let dataName' = DD.getFormDD dataName
  Utility.registerSwitcher (utilityHandle h) opacity dataName' $
    ResourceSpec {switch, arg, discard, copy, size = Utility.returnIntComp (utilityHandle h) (toInteger totalSlotCount), defaultValues = []}
  return $ C.UpIntro $ globalPointer dataName' AN.argNumS4

returnSigmaEnumS4 ::
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  IO C.Comp
returnSigmaEnumS4 h dataName opacity = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  let discard = C.UpIntro $ C.SigmaIntro []
  let copy = C.UpIntro argVar
  let dataName' = DD.getFormDD dataName
  Utility.registerSwitcher (utilityHandle h) opacity dataName' $
    ResourceSpec {switch, arg, discard, copy, size = Utility.returnIntComp (utilityHandle h) (-1), defaultValues = []}
  return $ C.UpIntro $ globalPointer dataName' AN.argNumS4

sigmaData4 :: Handle -> [DataConstructorInfo] -> C.Value -> IO C.Comp
sigmaData4 h = do
  sigmaData h (sigmaBinder4 h)

sigmaDataT :: Handle -> [DataConstructorInfo] -> C.Value -> IO C.Comp
sigmaDataT h = do
  sigmaData h (sigmaBinderT h)

sigmaBinder4 :: Handle -> DataConstructorInfo -> C.Value -> IO C.Comp
sigmaBinder4 h info v = do
  let xts = dataArgs info ++ consArgs info
  x <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
  sigmaData4' h (totalSlotCount info) ((x, returnImmediateIntS4 IntSize64) : xts) v

sigmaBinderT :: Handle -> DataConstructorInfo -> C.Value -> IO C.Comp
sigmaBinderT h info v = do
  let xts = dataArgs info ++ consArgs info
  x <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
  sigmaT h ((x, returnImmediateIntS4 IntSize64) : xts) v

sigmaData4' ::
  Handle ->
  Int ->
  [(Ident, C.Comp)] ->
  C.Value ->
  IO C.Comp
sigmaData4' h totalSlotCount xts argVar = do
  as <- forM xts $ \(x, t) -> do
    Utility.toRelevantApp (utilityHandle h) (C.VarLocal x) t
  (varNameList, varList) <- mapAndUnzipM (const $ Gensym.createVar (gensymHandle h) "pair") xts
  body' <- Linearize.linearize (linearizeHandle h) xts $ Utility.bindLet (zip varNameList as) $ C.UpIntro $ C.SigmaDataIntro totalSlotCount varList
  return $ C.SigmaElim False (map fst xts) argVar body'

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
          C.UpElim True disc (C.Primitive (C.Magic (LM.Load BLT.Pointer (C.VarLocal localName)))) enumElim

discriminantToEnumCase :: D.Discriminant -> EC.EnumCase
discriminantToEnumCase discriminant =
  EC.Int (D.reify discriminant)
