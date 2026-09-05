module Kernel.Clarify.Internal.Sigma
  ( Handle (..),
    DataConstructorInfo (..),
    FieldLayout (..),
    new,
    makeImmediateS4,
    makeClosureS4,
    immediateS4,
    closureS4,
    returnImmediateS4,
    returnClosureS4,
    closureEnvS4,
    returnSigmaDataS4,
    returnSigmaEnumS4,
    introCell,
    FieldSlots,
    fieldSlotVars,
    makeFieldSlotVars,
    bindFieldValues,
    bindFieldsInPlace,
  )
where

import Control.Monad
import Data.Maybe (mapMaybe)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Gensym.Handle qualified as Gensym
import Kernel.Clarify.Internal.Linearize qualified as Linearize
import Kernel.Clarify.Internal.Utility (ResourceSpec (..))
import Kernel.Clarify.Internal.Utility qualified as Utility
import Language.Common.ArgNum qualified as AN
import Language.Common.BaseLowType qualified as BLT
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DataInfo qualified as DI
import Language.Common.DataSize qualified as DS
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident
import Language.Common.LowMagic qualified as LM
import Language.Common.Opacity qualified as O
import Language.Common.CellLayout qualified as CL
import Language.Comp.Comp qualified as C
import Language.Comp.CreateVar qualified as Gensym
import Language.Comp.EnumCase qualified as EC

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    linearizeHandle :: Linearize.Handle,
    utilityHandle :: Utility.Handle,
    dataSize :: DS.DataSize
  }

data DataConstructorInfo = DataConstructorInfo
  { discriminant :: D.Discriminant,
    dataArgs :: [(Ident, C.Comp)],
    consArgs :: [(Ident, FieldLayout)],
    cellLayout :: CL.CellLayout
  }

data FieldLayout = FieldLayout
  { fieldType :: C.Comp,
    fieldShape :: CL.FieldStorage
  }

new :: Gensym.Handle -> Linearize.Handle -> Utility.Handle -> DS.DataSize -> Handle
new gensymHandle linearizeHandle utilityHandle dataSize = do
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
    ResourceSpec {switch, arg, extra, discard, copy, size = Utility.returnIntComp (-1), defaultValues = []}

makeClosureS4 :: Handle -> IO C.CompStmt
makeClosureS4 h = do
  (env, envVar) <- Gensym.createVar (gensymHandle h) "env"
  hole1 <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
  hole2 <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
  let xts = [(env, returnImmediateS4), (hole1, C.UpIntro envVar), (hole2, returnImmediateS4)]
  resourceSpec <- makeResourceSpecWithLayout h (DI.closureLayout (dataSize h)) xts
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

closureS4 :: C.Value
closureS4 =
  globalPointer DD.cls AN.argNumS4

makeResourceSpecWithLayout :: Handle -> CL.CellLayout -> [(Ident, C.Comp)] -> IO ResourceSpec
makeResourceSpecWithLayout h layout xts = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  extra@(_, extraVar) <- Gensym.createVar (gensymHandle h) "extra"
  discard <- sigmaT h layout xts argVar extraVar
  copy <- sigma4 h layout xts argVar extraVar
  let size = Utility.returnByteSizeComp (toInteger $ CL.cellByteSize layout)
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
  CL.CellLayout ->
  [(Ident, C.Comp)] ->
  C.Value ->
  C.Value ->
  IO C.Comp
sigmaT h layout xts argVar shouldRelease = do
  unitList <- forM xts $ \(x, t) -> do
    Utility.toAffineApp (utilityHandle h) (C.VarLocal x) t
  holeList <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "arg") xts
  let cont = freeOuterStorageIfRequested shouldRelease argVar layout (C.UpIntro C.null)
  body' <- Linearize.linearize (linearizeHandle h) xts $ Utility.bindLet (zip holeList unitList) cont
  return $ C.SigmaElim False 0 layout (map fst xts) argVar body'

-- sigma4 [(x1, t1), ..., (xn, tn)] arg dest   ~>
--   bind target = (dest == null) ? malloc(n words) : dest;
--   let-without-free (x1, ..., xn) = arg;
--   <linearize x1, ..., xn>;
--   bind _ = store (t1 @ (1, x1, null)) into target[0];    -- owned-copy each field, store into slot
--   ...
--   bind _ = store (tn @ (1, xn, null)) into target[n-1];
--   return target     -- owned: the freshly-allocated target; placed: the given dest (ignored by caller)
sigma4 :: Handle -> CL.CellLayout -> [(Ident, C.Comp)] -> C.Value -> C.Value -> IO C.Comp
sigma4 h layout xts argVar dest = do
  copyWithDestination h layout dest $ \target -> do
    copyEntries <- copyFieldsInto h target layout 0 (directEntries layout xts)
    holes <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "_") copyEntries
    body' <- Linearize.linearize (linearizeHandle h) xts $ Utility.bindLet (zip holes copyEntries) $ C.UpIntro C.null
    return $ C.SigmaElim False 0 layout (map fst xts) argVar body'

freeOuterStorageIfRequested :: C.Value -> C.Value -> CL.CellLayout -> C.Comp -> C.Comp
freeOuterStorageIfRequested shouldRelease value layout cont = do
  let byteSize = CL.cellByteSize layout
  if byteSize == 0
    then cont
    else
      C.EnumElim [] shouldRelease (C.Free value (Just byteSize) cont) [(EC.Int 0, cont)]

selectCopyDestination :: Handle -> CL.CellLayout -> C.Value -> IO C.Comp
selectCopyDestination h layout dest = do
  let byteSize = CL.cellByteSize layout
  if byteSize == 0
    then return $ C.UpIntro C.null
    else do
      (sizeName, sizeVar) <- Gensym.createVar (gensymHandle h) "size"
      let size = Utility.returnByteSizeComp (toInteger byteSize)
      let alloc = C.UpElim True sizeName size $ C.Primitive $ C.Alloc sizeVar
      return $ C.EnumElim [] dest (C.UpIntro dest) [(EC.Int 0, alloc)]

copyWithDestination :: Handle -> CL.CellLayout -> C.Value -> (C.Value -> IO C.Comp) -> IO C.Comp
copyWithDestination h layout dest fill = do
  targetName <- Gensym.newIdentFromText (gensymHandle h) "copy-dest"
  ignoredName <- Gensym.newIdentFromText (gensymHandle h) "_"
  target <- selectCopyDestination h layout dest
  fillTarget <- fill (C.VarLocal targetName)
  return $
    C.UpElim True targetName target $
      C.UpElim True ignoredName fillTarget $
        C.UpIntro (C.VarLocal targetName)

fieldAddress :: C.Value -> CL.CellLayout -> Int -> C.Comp
fieldAddress base layout slotIndex = do
  let (offset, _) = CL.cellSlots layout !! slotIndex
  C.Primitive $ C.ShiftPointer base (toInteger offset)

directEntries :: CL.CellLayout -> [(Ident, C.Comp)] -> [(Ident, FieldLayout)]
directEntries layout entries =
  zipWith (\(x, t) (_, width) -> (x, FieldLayout t (CL.StoredDirect width))) entries (CL.cellSlots layout)

copyDirectValueIntoField :: Handle -> C.Value -> CL.CellLayout -> Int -> CL.FieldWidth -> Ident -> C.Comp -> IO C.Comp
copyDirectValueIntoField h dest layout slotIndex width x t = do
  destSlotName <- Gensym.newIdentFromText (gensymHandle h) "dest-slot"
  copiedName <- Gensym.newIdentFromText (gensymHandle h) "copied"
  storeName <- Gensym.newIdentFromText (gensymHandle h) "_"
  copy <- Utility.toRelevantApp (utilityHandle h) (C.VarLocal x) t
  let destSlot = fieldAddress dest layout slotIndex
  let store = C.Primitive $ C.Magic $ LM.Store (CL.widthBaseLowType width) C.null (C.VarLocal copiedName) (C.VarLocal destSlotName)
  return $
    C.UpElim True destSlotName destSlot $
      C.UpElim True copiedName copy $
        C.UpElim True storeName store $
          C.UpIntro C.null

closureEnvS4 ::
  Handle ->
  DD.DefiniteDescription ->
  CL.CellLayout ->
  [(Ident, C.Comp)] ->
  [C.Value] ->
  IO C.Value
closureEnvS4 h closureName layout mxts defaultValues =
  case mxts of
    []
      | null defaultValues ->
          return immediateS4 -- performance optimization; not necessary for correctness
    _ -> do
      let name = DD.getClosureEnvDD closureName
      resourceSpec <- makeResourceSpecWithLayout h layout mxts
      let resourceSpec' = resourceSpec {defaultValues}
      liftIO $ Utility.registerSwitcher (utilityHandle h) O.Clear name resourceSpec'
      return $ globalPointer name AN.argNumS4

returnSigmaDataS4 ::
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  DI.CellShape ->
  [DataConstructorInfo] ->
  IO C.Comp
returnSigmaDataS4 h dataName opacity shape dataInfo = do
  switch <- Gensym.createVar (gensymHandle h) "switch"
  arg@(_, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  extra@(_, extraVar) <- Gensym.createVar (gensymHandle h) "extra"
  discard <- sigmaDataT h shape dataInfo argVar extraVar
  copy <- sigmaData4 h shape dataInfo argVar extraVar
  let dataName' = DD.getFormDD dataName
  Utility.registerSwitcher (utilityHandle h) opacity dataName' $ do
    let size = Utility.returnByteSizeComp (toInteger $ DI.shapeByteSize shape)
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
    ResourceSpec {switch, arg, extra, discard, copy, size = Utility.returnIntComp (-1), defaultValues = []}
  return $ C.UpIntro $ globalPointer dataName' AN.argNumS4

sigmaData ::
  Handle ->
  DI.CellShape ->
  (DataConstructorInfo -> C.Value -> IO C.Comp) ->
  [DataConstructorInfo] ->
  C.Value ->
  IO C.Comp
sigmaData h shape resourceHandler dataInfo arg = do
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
          C.UpElim True disc (C.Primitive (C.Magic (LM.Load (DI.discriminantLoadType shape) (C.VarLocal localName)))) enumElim

sigmaDataT :: Handle -> DI.CellShape -> [DataConstructorInfo] -> C.Value -> C.Value -> IO C.Comp
sigmaDataT h shape dataInfo arg shouldRelease = do
  sigmaData h shape (\info -> sigmaBinderT h shape info shouldRelease) dataInfo arg

sigmaData4 :: Handle -> DI.CellShape -> [DataConstructorInfo] -> C.Value -> C.Value -> IO C.Comp
sigmaData4 h shape dataInfo arg dest = do
  case dataInfo of
    [] ->
      return $ C.UpIntro arg
    info : _ -> do
      copyWithDestination h (cellLayout info) dest $ \target -> do
        sigmaData h shape (\info' -> sigmaBinder4 h shape info' target) dataInfo arg

-- discarder of one data constructor (layout: [disc | a1..ak | field1..fieldm]).
-- sigmaBinderT info shouldRelease v   ~>
--   let-without-free (disc, a1..ak, <field slots>) = v;
--   <linearize ...>;
--   bind _ = disc @ (0, _, 1);                -- owned-drop the header
--   bind _ = a_i @ (0, _, 1);                 -- owned-drop the type args
--   bind _ = <drop field_j>;
--   if shouldRelease == 1 { free v };         -- free the outer storage only when requested
--   return ()
sigmaBinderT :: Handle -> DI.CellShape -> DataConstructorInfo -> C.Value -> C.Value -> IO C.Comp
sigmaBinderT h shape info shouldRelease v = do
  headerEntries <- makeHeaderEntries h shape
  let dataArgEntries = dataArgs info
  let fields = consArgs info
  let readEntries = headerEntries ++ dataArgEntries
  readApps <- forM readEntries $ \(x, t) -> do
    Utility.toAffineApp (utilityHandle h) (C.VarLocal x) t
  fieldApps <- forM fields $ \(x, field) -> do
    case fieldShape field of
      CL.StoredDirect _ ->
        Utility.toAffineApp (utilityHandle h) (C.VarLocal x) (fieldType field)
      CL.StoredFlat _ ->
        Utility.toDropInPlaceAppWith (utilityHandle h) True (C.VarLocal x) (fieldType field)
  let as = readApps ++ fieldApps
  holes <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "arg") as
  let cont = freeOuterStorageIfRequested shouldRelease v (cellLayout info) (C.UpIntro C.null)
  let bodyBase = Utility.bindLet (zip holes as) cont
  let fieldStart = length headerEntries + length dataArgEntries
  bodyWithFields <- bindFieldsInPlace h v (cellLayout info) fieldStart (map (fmap fieldShape) fields) bodyBase
  body' <- Linearize.linearize (linearizeHandle h) readEntries bodyWithFields
  return $ C.SigmaElim False 0 (cellLayout info) (map fst readEntries) v body'

-- copier of one data constructor into dest (layout: [disc | a1..ak | field1..fieldm]).
-- sigmaBinder4 info dest v   ~>
--   let-without-free (disc, a1..ak, <field slots>) = v;
--   <linearize ...>;
--   bind _ = store (disc @ (1, _, null)) into dest[slot];   -- owned-copy the header, store into slot
--   bind _ = store (a_i @ (1, _, null)) into dest[slot];    -- owned-copy the type args, store into slot
--   bind _ = <copy field_j into dest>;
--   return ()
sigmaBinder4 :: Handle -> DI.CellShape -> DataConstructorInfo -> C.Value -> C.Value -> IO C.Comp
sigmaBinder4 h shape info dest v = do
  headerEntries <- makeHeaderEntries h shape
  let n = length headerEntries
  let dataArgEntries = dataArgs info
  let fields = consArgs info
  let readEntries = headerEntries ++ dataArgEntries
  readCopies <- copyFieldsInto h dest (cellLayout info) 0 (directEntries (cellLayout info) readEntries)
  fieldCopies <- copyFieldsInto h dest (cellLayout info) (n + length dataArgEntries) fields
  holes <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "_") (readCopies ++ fieldCopies)
  let bodyBase = Utility.bindLet (zip holes (readCopies ++ fieldCopies)) $ C.UpIntro C.null
  bodyWithFields <- bindFieldsInPlace h v (cellLayout info) (n + length dataArgEntries) (map (fmap fieldShape) fields) bodyBase
  body' <- Linearize.linearize (linearizeHandle h) readEntries bodyWithFields
  return $ C.SigmaElim False 0 (cellLayout info) (map fst headerEntries ++ map fst (dataArgs info)) v body'

makeHeaderEntries :: Handle -> DI.CellShape -> IO [(Ident, C.Comp)]
makeHeaderEntries h shape =
  case DI.shapeHeader shape of
    Nothing ->
      return []
    Just _ -> do
      disc <- Gensym.newIdentFromText (gensymHandle h) "unused-sigarg"
      return [(disc, returnImmediateS4)]

copyFieldsInto :: Handle -> C.Value -> CL.CellLayout -> Int -> [(Ident, FieldLayout)] -> IO [C.Comp]
copyFieldsInto h dest layout slotIndex fields = do
  case fields of
    [] ->
      return []
    (x, field) : rest -> do
      copy <- copyFieldInto h dest layout slotIndex x field
      rest' <- copyFieldsInto h dest layout (slotIndex + CL.storageSlotCount (fieldShape field)) rest
      return $ copy : rest'

copyFieldInto :: Handle -> C.Value -> CL.CellLayout -> Int -> Ident -> FieldLayout -> IO C.Comp
copyFieldInto h dest layout slotIndex x field =
  case fieldShape field of
    CL.StoredDirect width ->
      copyDirectValueIntoField h dest layout slotIndex width x (fieldType field)
    CL.StoredFlat _ -> do
      destSlotName <- Gensym.newIdentFromText (gensymHandle h) "dest-slot"
      let destSlot = fieldAddress dest layout slotIndex
      placedCopy <- Utility.toCopyIntoApp (utilityHandle h) (C.VarLocal x) (C.VarLocal destSlotName) (fieldType field)
      return $ C.UpElim True destSlotName destSlot placedCopy

data ChunkUnpack = ChunkUnpack CL.Chunks [Ident] C.Value

introCell :: Handle -> CL.CellLayout -> [(CL.FieldStorage, C.Value)] -> IO C.Comp
introCell h layout entries = do
  expansions <- mapM (expandCellEntry h) entries
  let values = concatMap fst expansions
  let unpacks = mapMaybe snd expansions
  return $ foldr unpackChunks (C.UpIntro (C.SigmaIntro layout values)) unpacks

expandCellEntry :: Handle -> (CL.FieldStorage, C.Value) -> IO ([C.Value], Maybe ChunkUnpack)
expandCellEntry h (storage, value) =
  case storage of
    CL.StoredDirect _ ->
      return ([value], Nothing)
    CL.StoredFlat chunks -> do
      chunkNames <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "chunk") chunks
      return (map C.VarLocal chunkNames, Just (ChunkUnpack chunks chunkNames value))

unpackChunks :: ChunkUnpack -> C.Comp -> C.Comp
unpackChunks (ChunkUnpack chunks chunkNames value) body =
  C.SigmaElim True 0 (CL.chunkCell chunks) chunkNames value body

data FieldSlots
  = DirectSlot Ident Ident
  | FlattenedSlots Ident CL.Chunks [Ident]

fieldSlotVars :: FieldSlots -> [Ident]
fieldSlotVars fieldSlots =
  case fieldSlots of
    DirectSlot _ slot ->
      [slot]
    FlattenedSlots _ _ slots ->
      slots

makeFieldSlotVars :: Gensym.Handle -> [(Ident, CL.FieldStorage)] -> IO [FieldSlots]
makeFieldSlotVars gensymHandle fields =
  case fields of
    [] ->
      return []
    (x, field) : rest -> do
      entry <-
        case field of
          CL.StoredDirect _ -> do
            slot <- Gensym.newIdentFromText gensymHandle "field"
            return $ DirectSlot x slot
          CL.StoredFlat chunks -> do
            slots <- mapM (const $ Gensym.newIdentFromText gensymHandle "field") chunks
            return $ FlattenedSlots x chunks slots
      rest' <- makeFieldSlotVars gensymHandle rest
      return $ entry : rest'

bindFieldValues :: [FieldSlots] -> C.Comp -> C.Comp
bindFieldValues fieldSlots body =
  case fieldSlots of
    [] ->
      body
    entry : rest -> do
      let body' = bindFieldValues rest body
      case entry of
        DirectSlot x slot ->
          C.UpElim True x (C.UpIntro (C.VarLocal slot)) body'
        FlattenedSlots x chunks slots ->
          C.UpElim False x (C.UpIntro (C.SigmaIntro (CL.chunkCell chunks) (map C.VarLocal slots))) body'

bindFieldsInPlace :: Handle -> C.Value -> CL.CellLayout -> Int -> [(Ident, CL.FieldStorage)] -> C.Comp -> IO C.Comp
bindFieldsInPlace h v layout slotIndex fields body =
  case fields of
    [] ->
      return body
    (x, field) : rest -> do
      rest' <- bindFieldsInPlace h v layout (slotIndex + CL.storageSlotCount field) rest body
      case field of
        CL.StoredDirect _ ->
          return $ C.SigmaElim False slotIndex layout [x] v rest'
        CL.StoredFlat _ ->
          return $ C.UpElim True x (fieldAddress v layout slotIndex) rest'

discriminantToEnumCase :: D.Discriminant -> EC.EnumCase
discriminantToEnumCase discriminant =
  EC.Int (D.reify discriminant)
