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
  extra <- Gensym.createVar (gensymHandle h) "extra"
  let discard = C.UpIntro C.null
  let copy = C.UpIntro argVar
  Utility.makeSwitcherStmt (utilityHandle h) O.Clear DD.imm $
    ResourceSpec {switch, arg, extra, discard, copy, size = Utility.returnIntComp (utilityHandle h) (-1), defaultValues = []}

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
  extra@(_, extraVar) <- Gensym.createVar (gensymHandle h) "extra"
  discard <- sigmaT h xts argVar extraVar
  copy <- sigma4 h xts argVar extraVar
  let size = Utility.returnByteSizeComp (utilityHandle h) (toInteger $ length xts)
  return $ ResourceSpec {switch, arg, extra, discard, copy, size, defaultValues = []}

-- sigmaT [(x1, t1), ..., (xn, tn)] arg shouldRelease   ~>
--   let-without-free (x1, ..., xn) = arg;
--   <linearize x1, ..., xn>;
--   bind _ = t1 @ (0, x1, 1);           -- drop each field (owned drop)
--   ...
--   bind _ = tn @ (0, xn, 1);
--   if shouldRelease == 1 { free arg };  -- free the outer storage only when requested
--   return ()
sigmaT ::
  Handle ->
  [(Ident, C.Comp)] ->
  C.Value ->
  C.Value ->
  IO C.Comp
sigmaT h xts argVar shouldRelease = do
  unitList <- forM xts $ \(x, t) -> do
    Utility.toAffineApp (utilityHandle h) (C.VarLocal x) t
  holeList <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "arg") xts
  cont <- freeOuterStorageIfRequested h shouldRelease argVar (length xts) (C.UpIntro C.null)
  body' <- Linearize.linearize (linearizeHandle h) xts $ Utility.bindLet (zip holeList unitList) cont
  return $ C.sigmaElim False (map fst xts) argVar body'

-- sigma4 [(x1, t1), ..., (xn, tn)] arg dest   ~>
--   bind target = (dest == null) ? malloc(n words) : dest;
--   let-without-free (x1, ..., xn) = arg;
--   <linearize x1, ..., xn>;
--   bind _ = store (t1 @ (1, x1, null)) into target[0];    -- owned-copy each field, store into slot
--   ...
--   bind _ = store (tn @ (1, xn, null)) into target[n-1];
--   return target     -- owned: the freshly-allocated target; placed: the given dest (ignored by caller)
sigma4 :: Handle -> [(Ident, C.Comp)] -> C.Value -> C.Value -> IO C.Comp
sigma4 h xts argVar dest = do
  copyWithDestination h (length xts) dest $ \target -> do
    copyEntries <- copyDirectEntriesInto h target (length xts) 0 xts
    holes <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "_") copyEntries
    body' <- Linearize.linearize (linearizeHandle h) xts $ Utility.bindLet (zip holes copyEntries) $ C.UpIntro C.null
    return $ C.sigmaElim False (map fst xts) argVar body'

freeOuterStorageIfRequested :: Handle -> C.Value -> C.Value -> Int -> C.Comp -> IO C.Comp
freeOuterStorageIfRequested h shouldRelease value slotCount cont = do
  if slotCount == 0
    then return cont
    else do
      let byteSize = fromInteger $ Utility.wordCountToByteSize (utilityHandle h) (toInteger slotCount)
      return $ C.EnumElim [] shouldRelease (C.Free value (Just byteSize) cont) [(EC.Int 0, cont)]

selectCopyDestination :: Handle -> Int -> C.Value -> IO C.Comp
selectCopyDestination h slotCount dest = do
  if slotCount == 0
    then return $ C.UpIntro C.null
    else do
      (sizeName, sizeVar) <- Gensym.createVar (gensymHandle h) "size"
      let size = Utility.returnByteSizeComp (utilityHandle h) (toInteger slotCount)
      let alloc = C.UpElim True sizeName size $ C.Primitive $ C.Alloc sizeVar
      return $ C.EnumElim [] dest (C.UpIntro dest) [(EC.Int 0, alloc)]

copyWithDestination :: Handle -> Int -> C.Value -> (C.Value -> IO C.Comp) -> IO C.Comp
copyWithDestination h slotCount dest fill = do
  targetName <- Gensym.newIdentFromText (gensymHandle h) "copy-dest"
  ignoredName <- Gensym.newIdentFromText (gensymHandle h) "_"
  target <- selectCopyDestination h slotCount dest
  fillTarget <- fill (C.VarLocal targetName)
  return $
    C.UpElim True targetName target $
      C.UpElim True ignoredName fillTarget $
        C.UpIntro (C.VarLocal targetName)

copyDirectValueIntoSlot :: Handle -> C.Value -> Int -> Int -> Ident -> C.Comp -> IO C.Comp
copyDirectValueIntoSlot h dest totalSlots cursor x t = do
  destSlotName <- Gensym.newIdentFromText (gensymHandle h) "dest-slot"
  copiedName <- Gensym.newIdentFromText (gensymHandle h) "copied"
  storeName <- Gensym.newIdentFromText (gensymHandle h) "_"
  copy <- Utility.toRelevantApp (utilityHandle h) (C.VarLocal x) t
  let destSlot = C.Primitive $ C.ShiftPointer dest (toInteger totalSlots) (toInteger cursor)
  let store = C.Primitive $ C.Magic $ LM.Store BLT.Pointer C.null (C.VarLocal copiedName) (C.VarLocal destSlotName)
  return $
    C.UpElim True destSlotName destSlot $
      C.UpElim True copiedName copy $
        C.UpElim True storeName store $
          C.UpIntro C.null

copyDirectEntriesInto :: Handle -> C.Value -> Int -> Int -> [(Ident, C.Comp)] -> IO [C.Comp]
copyDirectEntriesInto h dest totalSlots cursor entries = do
  case entries of
    [] ->
      return []
    (x, t) : rest -> do
      copy <- copyDirectValueIntoSlot h dest totalSlots cursor x t
      rest' <- copyDirectEntriesInto h dest totalSlots (cursor + 1) rest
      return $ copy : rest'

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
      let name = DD.getClosureEnvDD closureName
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
  extra@(_, extraVar) <- Gensym.createVar (gensymHandle h) "extra"
  discard <- sigmaDataT h dataInfo argVar extraVar
  copy <- sigmaData4 h dataInfo argVar extraVar
  let dataName' = DD.getFormDD dataName
  Utility.registerSwitcher (utilityHandle h) opacity dataName' $ do
    let size = Utility.returnByteSizeComp (utilityHandle h) (toInteger totalSlotCount)
    ResourceSpec {switch, arg, extra, discard, copy, size, defaultValues = []}
  return $ C.UpIntro $ globalPointer dataName' AN.argNumS4

returnSigmaEnumS4 ::
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  IO C.Comp
returnSigmaEnumS4 h dataName opacity = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  extra <- Gensym.createVar (gensymHandle h) "extra"
  let discard = C.UpIntro C.null
  let copy = C.UpIntro argVar
  let dataName' = DD.getFormDD dataName
  Utility.registerSwitcher (utilityHandle h) opacity dataName' $
    ResourceSpec {switch, arg, extra, discard, copy, size = Utility.returnIntComp (utilityHandle h) (-1), defaultValues = []}
  return $ C.UpIntro $ globalPointer dataName' AN.argNumS4

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

sigmaDataT :: Handle -> [DataConstructorInfo] -> C.Value -> C.Value -> IO C.Comp
sigmaDataT h dataInfo arg shouldRelease = do
  sigmaData h (\info -> sigmaBinderT h info shouldRelease) dataInfo arg

sigmaData4 :: Handle -> [DataConstructorInfo] -> C.Value -> C.Value -> IO C.Comp
sigmaData4 h dataInfo arg dest = do
  case dataInfo of
    [] ->
      return $ C.UpIntro arg
    info : _ -> do
      copyWithDestination h (totalSlotCount info) dest $ \target -> do
        sigmaData h (\info' -> sigmaBinder4 h info' target) dataInfo arg

-- discarder of one data constructor (layout: [disc | a1..ak | field1..fieldm]).
-- sigmaBinderT info shouldRelease v   ~>
--   let-without-free (disc, a1..ak, <field slots>) = v;
--   <linearize ...>;
--   bind _ = disc @ (0, _, 1);                -- owned-drop the header
--   bind _ = a_i @ (0, _, 1);                 -- owned-drop the type args
--   bind _ = <drop field_j>;
--   if shouldRelease == 1 { free v };         -- free the outer storage only when requested
--   return ()
sigmaBinderT :: Handle -> DataConstructorInfo -> C.Value -> C.Value -> IO C.Comp
sigmaBinderT h info shouldRelease v = do
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
    case field of
      Direct t ->
        Utility.toAffineApp (utilityHandle h) (C.VarLocal x) t
      Flattened t _ ->
        Utility.toDropInPlaceAppWith (utilityHandle h) True (C.VarLocal x) t
  let as = readApps ++ fieldApps
  holes <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "arg") valueEntries
  cont <- freeOuterStorageIfRequested h shouldRelease v (totalSlotCount info) (C.UpIntro C.null)
  let bodyBase = Utility.bindLet (zip holes as) cont
  let bodyWithFields = bindFieldValues fieldSlots bodyBase
  body' <- Linearize.linearize (linearizeHandle h) readEntries bodyWithFields
  return $ C.SigmaElim False 0 (totalSlotCount info) readVars v body'

-- copier of one data constructor into dest (layout: [disc | a1..ak | field1..fieldm]).
-- sigmaBinder4 info dest v   ~>
--   let-without-free (disc, a1..ak, <field slots>) = v;
--   <linearize ...>;
--   bind _ = store (disc @ (1, _, null)) into dest[slot];   -- owned-copy the header, store into slot
--   bind _ = store (a_i @ (1, _, null)) into dest[slot];    -- owned-copy the type args, store into slot
--   bind _ = <copy field_j into dest>;
--   return ()
sigmaBinder4 :: Handle -> DataConstructorInfo -> C.Value -> C.Value -> IO C.Comp
sigmaBinder4 h info dest v = do
  headerEntries <- makeHeaderEntries h (headerSize info)
  let n = length headerEntries
  let dataArgEntries = dataArgs info
  let fields = consArgs info
  let readEntries = headerEntries ++ dataArgEntries
  readCopies <- copyDirectEntriesInto h dest (totalSlotCount info) 0 readEntries
  fieldCopies <- copyFieldsInto h dest (totalSlotCount info) (n + length dataArgEntries) fields
  holes <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "_") (readCopies ++ fieldCopies)
  let bodyBase = Utility.bindLet (zip holes (readCopies ++ fieldCopies)) $ C.UpIntro C.null
  let bodyWithFields = bindFieldsInPlace v (totalSlotCount info) (n + length dataArgEntries) fields bodyBase
  body' <- Linearize.linearize (linearizeHandle h) readEntries bodyWithFields
  return $ C.SigmaElim False 0 (totalSlotCount info) (map fst headerEntries ++ map fst (dataArgs info)) v body'

makeHeaderEntries :: Handle -> Int -> IO [(Ident, C.Comp)]
makeHeaderEntries h headerSize =
  if headerSize == 0
    then return []
    else do
      disc <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
      return [(disc, returnImmediateS4)]

copyFieldsInto :: Handle -> C.Value -> Int -> Int -> [(Ident, FieldLayout)] -> IO [C.Comp]
copyFieldsInto h dest totalSlots cursor fields = do
  case fields of
    [] ->
      return []
    (x, field) : rest -> do
      copy <-
        case field of
          Direct t -> do
            copyDirectValueIntoSlot h dest totalSlots cursor x t
          Flattened t _ -> do
            destSlotName <- Gensym.newIdentFromText (gensymHandle h) "dest-slot"
            let destSlot = C.Primitive $ C.ShiftPointer dest (toInteger totalSlots) (toInteger cursor)
            placedCopy <- Utility.toCopyIntoApp (utilityHandle h) (C.VarLocal x) (C.VarLocal destSlotName) t
            return $ C.UpElim True destSlotName destSlot placedCopy
      rest' <- copyFieldsInto h dest totalSlots (cursor + fieldSlotCount field) rest
      return $ copy : rest'

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

discriminantToEnumCase :: D.Discriminant -> EC.EnumCase
discriminantToEnumCase discriminant =
  EC.Int (D.reify discriminant)
