module Scene.Emit (emit) where

import Context.App
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Builder
import Data.ByteString.Builder qualified as L
import Data.ByteString.Lazy qualified as L
import Data.HashMap.Strict qualified as HashMap
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Entity.BaseLowType qualified as BLT
import Entity.Builder
import Entity.Const
import Entity.DataSize qualified as DS
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.ForeignCodType qualified as FCT
import Entity.Ident
import Entity.Ident.Reify
import Entity.LowComp qualified as LC
import Entity.LowComp.EmitOp qualified as EOP
import Entity.LowComp.EmitValue
import Entity.LowType qualified as LT
import Entity.LowType.EmitLowType
import Entity.LowType.FromBaseLowType qualified as LT
import Entity.PrimNumSize
import Entity.PrimType qualified as PT
import Entity.PrimType.EmitPrimType (emitPrimType)
import Scene.LowComp.Reduce qualified as LowComp

emit :: LC.LowCode -> App L.ByteString
emit lowCode = do
  case lowCode of
    LC.LowCodeMain mainDef lowCodeInfo -> do
      main <- emitMain mainDef
      let argDef = emitArgDef
      (header, body) <- emitLowCodeInfo lowCodeInfo
      return $ buildByteString $ header ++ argDef ++ main ++ body
    LC.LowCodeNormal lowCodeInfo -> do
      let argDecl = emitArgDecl
      (header, body) <- emitLowCodeInfo lowCodeInfo
      return $ buildByteString $ header ++ argDecl ++ body

emitLowCodeInfo :: LC.LowCodeInfo -> App ([Builder], [Builder])
emitLowCodeInfo (declEnv, defList, staticTextList) = do
  baseSize <- Env.getBaseSize'
  let declStrList = emitDeclarations declEnv
  let staticTextList' = map (emitStaticText baseSize) staticTextList
  defStrList <- concat <$> mapM emitDefinitions defList
  return (declStrList <> staticTextList', defStrList)

emitArgDecl :: [Builder]
emitArgDecl = do
  let argc = emitGlobalExt unsafeArgcName LT.Pointer
  let argv = emitGlobalExt unsafeArgvName LT.Pointer
  [argc, argv]

emitArgDef :: [Builder]
emitArgDef = do
  let argc = emitGlobal unsafeArgcName LT.Pointer LC.Null
  let argv = emitGlobal unsafeArgvName LT.Pointer LC.Null
  [argc, argv]

buildByteString :: [Builder] -> L.ByteString
buildByteString =
  L.toLazyByteString . unlinesL

emitGlobal :: T.Text -> LT.LowType -> LC.Value -> Builder
emitGlobal name lt v =
  "@"
    <> TE.encodeUtf8Builder name
    <> " = global "
    <> emitLowType lt
    <> " "
    <> emitValue v

emitGlobalExt :: T.Text -> LT.LowType -> Builder
emitGlobalExt name lt =
  "@"
    <> TE.encodeUtf8Builder name
    <> " = external global "
    <> emitLowType lt

type StaticTextInfo = (DD.DefiniteDescription, (Builder, Int))

emitStaticText :: Int -> StaticTextInfo -> Builder
emitStaticText baseSize (from, (text, len)) = do
  "@"
    <> DD.toBuilder from
    <> " = private unnamed_addr constant "
    <> emitLowType (LT.textType baseSize len)
    <> " {"
    <> "i"
    <> intDec baseSize
    <> " 0, "
    <> "i"
    <> intDec baseSize
    <> " "
    <> intDec len
    <> ", "
    <> emitLowType (LT.textTypeInner len)
    <> " c\""
    <> text
    <> "\""
    <> "}"

emitDeclarations :: DN.DeclEnv -> [Builder]
emitDeclarations declEnv = do
  map declToBuilder $ List.sort $ HashMap.toList declEnv

emitDefinitions :: LC.Def -> App [Builder]
emitDefinitions (name, (args, body)) = do
  args' <- mapM Gensym.newIdentFromIdent args
  let sub = IntMap.fromList $ zipWith (\from to -> (toInt from, LC.VarLocal to)) args args'
  body' <- LowComp.reduce sub body
  let args'' = map (emitValue . LC.VarLocal) args'
  emitDefinition "ptr" (DD.toBuilder name) args'' body'

getMainType :: App Builder
getMainType = do
  dataSize <- Env.getDataSize'
  return $ emitPrimType $ PT.Int (IntSize $ DS.reify dataSize)

emitMain :: LC.DefContent -> App [Builder]
emitMain (args, body) = do
  mainType <- getMainType
  let args' = map (emitValue . LC.VarLocal) args
  emitDefinition mainType "main" args' body

declToBuilder :: (DN.DeclarationName, ([BLT.BaseLowType], FCT.ForeignCodType BLT.BaseLowType)) -> Builder
declToBuilder (name, (dom, cod)) = do
  let name' = DN.toBuilder name
  "declare "
    <> emitLowType (FCT.fromForeignCodType cod)
    <> " @"
    <> name'
    <> "("
    <> unwordsC (map (emitLowType . LT.fromBaseLowType) dom)
    <> ")"

emitDefinition :: Builder -> Builder -> [Builder] -> LC.Comp -> App [Builder]
emitDefinition retType name args asm = do
  let header = sig retType name args <> " {"
  ctx <- newCtx retType
  content <- emitLowComp ctx asm
  let footer = "}"
  return $ [header] <> content <> [footer]

sig :: Builder -> Builder -> [Builder] -> Builder
sig retType name args =
  "define fastcc " <> retType <> " @" <> name <> showFuncArgs args

type Label =
  Ident

data EmitCtx = EmitCtx
  { phiInfo :: Maybe (Ident, Label),
    currentLabel :: Maybe Label,
    retType :: Builder,
    labelMap :: IORef (IntMap.IntMap Ident),
    axis :: EOP.Axis
  }

newCtx :: Builder -> App EmitCtx
newCtx retTypeBuilder = do
  emptyMapRef <- liftIO $ newIORef IntMap.empty
  baseSize <- Env.getBaseSize'
  let lowInt = LT.PrimNum $ PT.Int $ IntSize baseSize
  return
    EmitCtx
      { phiInfo = Nothing,
        currentLabel = Nothing,
        retType = retTypeBuilder,
        labelMap = emptyMapRef,
        axis = EOP.Axis {intType = lowInt}
      }

emitLowComp :: EmitCtx -> LC.Comp -> App [Builder]
emitLowComp ctx lowComp =
  case lowComp of
    LC.Return d ->
      case phiInfo ctx of
        Nothing ->
          return $ emitOp $ unwordsL ["ret", retType ctx, emitValue d]
        Just (phiSrcVar, rendezvous) -> do
          let lowOp = emitLowOp (axis ctx) (emitValue (LC.VarLocal phiSrcVar) <> " = ") $ LC.Bitcast d LT.Pointer LT.Pointer
          let brOp = emitOp $ unwordsL ["br", "label", emitValue (LC.VarLocal rendezvous)]
          return $ lowOp <> brOp
    LC.TailCall codType f args -> do
      tmp <- Gensym.newIdentFromText "tmp"
      let op =
            emitOp $
              unwordsL
                [ emitValue (LC.VarLocal tmp),
                  "=",
                  "tail call fastcc",
                  emitLowType codType,
                  emitValue f <> showArgs args
                ]
      ret <- emitLowComp ctx $ LC.Return (LC.VarLocal tmp)
      return $ op <> ret
    LC.Switch (d, lowType) defaultBranch branchList (phiTgt, cont) -> do
      defaultLabel <- Gensym.newIdentFromText "default"
      labelList <- constructLabelList branchList
      let switchOpStr =
            emitOp $
              unwordsL
                [ "switch",
                  emitLowType lowType,
                  emitValue d <> ",",
                  "label",
                  emitValue (LC.VarLocal defaultLabel),
                  showBranchList lowType $ zip (map fst branchList) labelList
                ]
      let labelBranchList = zip labelList (map snd branchList) <> [(defaultLabel, defaultBranch)]
      confluenceLabel <- Gensym.newIdentFromText "confluence"
      case currentLabel ctx of
        Nothing ->
          return ()
        Just current -> do
          -- bypass switch clauses and get the confluence block
          liftIO $ modifyIORef' (labelMap ctx) $ IntMap.insert (toInt current) confluenceLabel
      phiSrcVarList <- mapM (const $ Gensym.newIdentFromText "phi") labelBranchList
      blockAsmList <-
        forM (zip labelBranchList phiSrcVarList) $ \((label, branch), phiSrcVar) -> do
          let newPhiInfo = Just (phiSrcVar, confluenceLabel)
          a <- emitLowComp (ctx {phiInfo = newPhiInfo, currentLabel = Just label}) branch
          return $ emitLabel ("_" <> intDec (toInt label)) : a
      let allLabelList = map fst labelBranchList
      currentLabelMap <- liftIO $ readIORef $ labelMap ctx
      let resolvedLabelList = resolveLabelList currentLabelMap allLabelList
      let phiOp =
            unwordsL
              [ "phi",
                emitLowType LT.Pointer,
                emitPhiList $ zip phiSrcVarList resolvedLabelList
              ]
      let phiOpStr = emitOp $ emitValue (LC.VarLocal phiTgt) <> " = " <> phiOp
      rendezvousBlock <- do
        a <- emitLowComp (ctx {currentLabel = Just confluenceLabel}) cont
        return $ emitLabel ("_" <> intDec (toInt confluenceLabel)) : phiOpStr <> a
      return $ switchOpStr <> concat blockAsmList <> rendezvousBlock
    LC.Cont op cont -> do
      let lowOp = emitLowOp (axis ctx) "" op
      a <- emitLowComp ctx cont
      return $ lowOp <> a
    LC.Let x op cont -> do
      let lowOp = emitLowOp (axis ctx) (emitValue (LC.VarLocal x) <> " = ") op
      a <- emitLowComp ctx cont
      return $ lowOp <> a
    LC.Unreachable -> do
      emitLowComp ctx $ LC.Return LC.Null

resolveLabelList :: IntMap.IntMap Ident -> [Ident] -> [Ident]
resolveLabelList labelMap xs =
  case xs of
    [] ->
      []
    x : rest ->
      case IntMap.lookup (toInt x) labelMap of
        Just y ->
          resolveLabelList labelMap $ y : rest
        Nothing ->
          x : resolveLabelList labelMap rest

emitLowOp :: EOP.Axis -> Builder -> LC.Op -> [Builder]
emitLowOp ax prefix op = do
  emitOp $ prefix <> EOP.emitLowOp ax op

emitPhiList :: [(Ident, Ident)] -> Builder
emitPhiList identLabelList =
  case identLabelList of
    [] ->
      ""
    [(ident, label)] ->
      "[" <> emitValue (LC.VarLocal ident) <> ", " <> emitValue (LC.VarLocal label) <> "]"
    (ident, label) : rest ->
      showIdentLabel ident label <> ", " <> emitPhiList rest

showIdentLabel :: Ident -> Ident -> Builder
showIdentLabel ident label =
  "[" <> emitValue (LC.VarLocal ident) <> ", " <> emitValue (LC.VarLocal label) <> "]"

emitOp :: Builder -> [Builder]
emitOp s =
  ["  " <> s]

emitLabel :: Builder -> Builder
emitLabel s =
  s <> ":"

constructLabelList :: [a] -> App [Ident]
constructLabelList input =
  case input of
    [] ->
      return []
    (_ : rest) -> do
      label <- Gensym.newIdentFromText "case"
      labelList <- constructLabelList rest
      return $ label : labelList

showBranchList :: LT.LowType -> [(Integer, Ident)] -> Builder
showBranchList lowType xs =
  "[" <> unwordsL (map (uncurry (showBranch lowType)) xs) <> "]"

showBranch :: LT.LowType -> Integer -> Ident -> Builder
showBranch lowType i label =
  emitLowType lowType
    <> " "
    <> integerDec i
    <> ", label "
    <> emitValue (LC.VarLocal label)
