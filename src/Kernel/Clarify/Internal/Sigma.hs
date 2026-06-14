module Kernel.Clarify.Internal.Sigma
  ( Handle (..),
    DataConstructorInfo (..),
    FieldLayout (..),
    FieldSlots,
    fieldSlotCount,
    fieldSlotVars,
    new,
    makeImmediateS4,
    makeClosureS4,
    immediateS4,
    returnImmediateS4,
    returnClosureS4,
    closureEnvS4,
    returnSigmaDataS4,
    returnSigmaEnumS4,
    makeFieldSlotVars,
    bindFieldValues,
    bindFieldsInPlace,
    flattenFields,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
import Kernel.Clarify.Internal.Linearize qualified as Linearize
import Kernel.Clarify.Internal.Utility (ResourceSpec (..))
import Kernel.Clarify.Internal.Utility qualified as Utility
import Language.Common.ArgNum qualified as AN
import Language.Common.BaseLowType qualified as BLT
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident
import Language.Common.LowMagic qualified as LM
import Language.Common.Opacity qualified as O
import Language.Comp.Comp qualified as C
import Language.Comp.CreateVar qualified as Gensym
import Language.Comp.EnumCase qualified as EC

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    linearizeHandle :: Linearize.Handle,
    utilityHandle :: Utility.Handle
  }

data DataConstructorInfo = DataConstructorInfo
  { discriminant :: D.Discriminant,
    dataArgs :: [(Ident, C.Comp)],
    consArgs :: [(Ident, FieldLayout)],
    headerSize :: Int,
    totalSlotCount :: Int
  }

data FieldLayout
  = Direct C.Comp
  | Flattened C.Comp Int

fieldType :: FieldLayout -> C.Comp
fieldType field =
  case field of
    Direct t ->
      t
    Flattened t _ ->
      t

fieldSlotCount :: FieldLayout -> Int
fieldSlotCount field =
  case field of
    Direct _ ->
      1
    Flattened _ slotCount ->
      slotCount

toAffineFieldApp :: Handle -> Ident -> FieldLayout -> IO C.Comp
toAffineFieldApp h x field =
  case field of
    Direct t ->
      Utility.toAffineApp (utilityHandle h) (C.VarLocal x) t
    Flattened t _ ->
      Utility.toAffineAppWith (utilityHandle h) True (C.VarLocal x) t

toRelevantFieldApp :: Handle -> Ident -> FieldLayout -> IO C.Comp
toRelevantFieldApp h x field =
  case field of
    Direct t ->
      Utility.toRelevantApp (utilityHandle h) (C.VarLocal x) t
    Flattened t _ ->
      Utility.toRelevantAppWith (utilityHandle h) True (C.VarLocal x) t

data FieldSlots
  = DirectSlot Ident Ident
  | FlattenedSlots Ident [Ident]

fieldSlotArgName :: FieldSlots -> Ident
fieldSlotArgName fieldSlots =
  case fieldSlots of
    DirectSlot x _ ->
      x
    FlattenedSlots x _ ->
      x

fieldSlotVars :: FieldSlots -> [Ident]
fieldSlotVars fieldSlots =
  case fieldSlots of
    DirectSlot _ slot ->
      [slot]
    FlattenedSlots _ slots ->
      slots

new :: Gensym.Handle -> Linearize.Handle -> Utility.Handle -> Handle
new gensymHandle linearizeHandle utilityHandle = do
  Handle {..}

globalPointer :: DD.DefiniteDescription -> AN.ArgNum -> C.Value
globalPointer name argNum =
  C.VarGlobal name argNum (FCT.Cod BLT.Pointer)

makeImmediateS4 :: Handle -> IO C.CompStmt
makeImmediateS4 h = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  let discard = C.UpIntro C.null
  let copy = C.UpIntro argVar
  Utility.makeSwitcherStmt (utilityHandle h) O.Clear DD.imm $
    ResourceSpec {switch, arg, discard, copy, size = Utility.returnIntComp (utilityHandle h) (-1), defaultValues = []}

makeClosureS4 :: Handle -> IO C.CompStmt
makeClosureS4 h = do
  (env, envVar) <- Gensym.createVar (gensymHandle h) "env"
  hole1 <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
  hole2 <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
  let xts = [(env, returnImmediateS4), (hole1, C.UpIntro envVar), (hole2, returnImmediateS4)]
  resourceSpec <- makeSigmaResourceSpec h xts
  Utility.makeSwitcherStmt (utilityHandle h) O.Clear DD.cls resourceSpec

returnImmediateS4 :: C.Comp
returnImmediateS4 =
  C.UpIntro immediateS4

returnClosureS4 :: C.Comp
returnClosureS4 = do
  C.UpIntro $ globalPointer DD.cls AN.argNumS4

immediateS4 :: C.Value
immediateS4 =
  globalPointer DD.imm AN.argNumS4

makeSigmaResourceSpec :: Handle -> [(Ident, C.Comp)] -> IO ResourceSpec
makeSigmaResourceSpec h xts = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  discard <- sigmaT h xts argVar
  copy <- sigma4 h xts argVar
  let size = Utility.returnByteSizeComp (utilityHandle h) (toInteger $ length xts)
  return $ ResourceSpec {switch, arg, discard, copy, size, defaultValues = []}

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
  body' <- Linearize.linearize (linearizeHandle h) xts $ Utility.bindLet (zip ys as) $ C.UpIntro C.null
  return $ C.sigmaElim True (map fst xts) argVar body'

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
  body' <- Linearize.linearize (linearizeHandle h) xts $ Utility.bindLet (zip varNameList as) $ C.UpIntro $ C.sigmaIntro varList
  return $ C.sigmaElim False (map fst xts) argVar body'

closureEnvS4 ::
  Handle ->
  DD.DefiniteDescription ->
  [(Ident, C.Comp)] ->
  [C.Value] ->
  IO C.Value
closureEnvS4 h closureName mxts defaultValues =
  case mxts of
    []
      | null defaultValues ->
          return immediateS4 -- performance optimization; not necessary for correctness
    _ -> do
      i <- Gensym.newCount (gensymHandle h)
      let name = DD.getClosureEnvDD closureName i
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
    ResourceSpec {switch, arg, discard, copy, size = Utility.returnByteSizeComp (utilityHandle h) (toInteger totalSlotCount), defaultValues = []}
  return $ C.UpIntro $ globalPointer dataName' AN.argNumS4

returnSigmaEnumS4 ::
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  IO C.Comp
returnSigmaEnumS4 h dataName opacity = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  let discard = C.UpIntro C.null
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

makeHeaderEntries :: Handle -> Int -> IO [(Ident, C.Comp)]
makeHeaderEntries h headerSize =
  if headerSize == 0
    then return []
    else do
      disc <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
      return [(disc, returnImmediateS4)]

sigmaBinder4 :: Handle -> DataConstructorInfo -> C.Value -> IO C.Comp
sigmaBinder4 h info v = do
  headerEntries <- makeHeaderEntries h (headerSize info)
  let n = length headerEntries
  let dataArgEntries = dataArgs info
  let fields = consArgs info
  let fieldEntries = map (\(x, field) -> (x, fieldType field)) fields
  let readEntries = headerEntries ++ dataArgEntries
  let valueEntries = readEntries ++ fieldEntries
  readApps <- forM readEntries $ \(x, t) -> do
    Utility.toRelevantApp (utilityHandle h) (C.VarLocal x) t
  fieldApps <- forM fields $ \(x, field) -> do
    toRelevantFieldApp h x field
  let as = readApps ++ fieldApps
  (copyNames, copyValues) <- mapAndUnzipM (const $ Gensym.createVar (gensymHandle h) "pair") valueEntries
  let copiedHeader = take n copyValues
  let copiedDataArgs = take (length dataArgEntries) $ drop n copyValues
  bodyAfterFlatten <- do
    let fieldValues = zip (map snd fields) (drop (n + length dataArgEntries) copyValues)
    flattenFields (gensymHandle h) fieldValues $ \resultSlots ->
      C.UpIntro $ C.SigmaIntro (totalSlotCount info) (copiedHeader ++ copiedDataArgs ++ resultSlots)
  let bodyBase = Utility.bindLet (zip copyNames as) bodyAfterFlatten
  let fieldStart = n + length dataArgEntries
  let bodyWithFields = bindFieldsInPlace v (totalSlotCount info) fieldStart fields bodyBase
  body' <- Linearize.linearize (linearizeHandle h) readEntries bodyWithFields
  return $ C.SigmaElim False 0 (totalSlotCount info) (map fst headerEntries ++ map fst (dataArgs info)) v body'

sigmaBinderT :: Handle -> DataConstructorInfo -> C.Value -> IO C.Comp
sigmaBinderT h info v = do
  headerEntries <- makeHeaderEntries h (headerSize info)
  fieldSlots <- makeFieldSlotVars (gensymHandle h) (consArgs info)
  let fieldSlotNames = concatMap fieldSlotVars fieldSlots
  let readVars = map fst headerEntries ++ map fst (dataArgs info) ++ fieldSlotNames
  let dataArgEntries = dataArgs info
  let fields = consArgs info
  let fieldEntries = map (\(x, field) -> (x, fieldType field)) fields
  let readEntries = headerEntries ++ dataArgEntries
  let valueEntries = readEntries ++ fieldEntries
  readApps <- forM readEntries $ \(x, t) -> do
    Utility.toAffineApp (utilityHandle h) (C.VarLocal x) t
  fieldApps <- forM fields $ \(x, field) -> do
    toAffineFieldApp h x field
  let as = readApps ++ fieldApps
  holes <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "arg") valueEntries
  let bodyBase = Utility.bindLet (zip holes as) $ C.UpIntro C.null
  let bodyWithFields = bindFieldValues fieldSlots bodyBase
  body' <- Linearize.linearize (linearizeHandle h) readEntries bodyWithFields
  return $ C.SigmaElim True 0 (totalSlotCount info) readVars v body'

makeFieldSlotVars :: Gensym.Handle -> [(Ident, FieldLayout)] -> IO [FieldSlots]
makeFieldSlotVars gensymHandle fields =
  case fields of
    [] ->
      return []
    (x, field) : rest -> do
      entry <-
        case field of
          Direct _ -> do
            slot <- Gensym.newIdentFromText gensymHandle "field"
            return $ DirectSlot x slot
          Flattened _ slotCount -> do
            slots <- mapM (const $ Gensym.newIdentFromText gensymHandle "field") [1 .. slotCount]
            return $ FlattenedSlots x slots
      rest' <- makeFieldSlotVars gensymHandle rest
      return $ entry : rest'

bindFieldValues :: [FieldSlots] -> C.Comp -> C.Comp
bindFieldValues fieldSlots body =
  case fieldSlots of
    [] ->
      body
    entry : rest -> do
      let value =
            case entry of
              DirectSlot _ slot ->
                C.VarLocal slot
              FlattenedSlots _ slots ->
                C.SigmaIntro (length slots) (map C.VarLocal slots)
      C.UpElim True (fieldSlotArgName entry) (C.UpIntro value) (bindFieldValues rest body)

bindFieldsInPlace :: C.Value -> Int -> Int -> [(Ident, FieldLayout)] -> C.Comp -> C.Comp
bindFieldsInPlace v totalSlots fieldStart fields body =
  case fields of
    [] ->
      body
    (x, field) : rest -> do
      let rest' = bindFieldsInPlace v totalSlots (fieldStart + fieldSlotCount field) rest body
      case field of
        Direct _ ->
          C.SigmaElim False fieldStart totalSlots [x] v rest'
        Flattened _ _ ->
          C.UpElim True x (C.Primitive (C.ShiftPointer v (toInteger totalSlots) (toInteger fieldStart))) rest'

flattenFields :: Gensym.Handle -> [(FieldLayout, C.Value)] -> ([C.Value] -> C.Comp) -> IO C.Comp
flattenFields gensymHandle fields cont =
  case fields of
    [] ->
      return $ cont []
    (field, value) : rest -> do
      case field of
        Direct _ ->
          flattenFields gensymHandle rest $ \restSlots -> cont (value : restSlots)
        Flattened _ slotCount -> do
          slotNames <- mapM (const $ Gensym.newIdentFromText gensymHandle "field") [1 .. slotCount]
          let slotValues = map C.VarLocal slotNames
          body <- flattenFields gensymHandle rest $ \restSlots -> cont (slotValues ++ restSlots)
          return $ C.SigmaElim True 0 slotCount slotNames value body

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
    [info] ->
      resourceHandler info arg
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
