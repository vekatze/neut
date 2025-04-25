module Move.Scene.Emit.LowComp
  ( Handle,
    new,
    emitLowComp,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Builder
import Data.IORef
import Data.IntMap qualified as IntMap
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Gensym qualified as Gensym
import Move.Language.Utility.Gensym qualified as GensymN
import Rule.Builder
import Rule.Ident
import Rule.Ident.Reify
import Rule.LowComp qualified as LC
import Rule.LowComp.EmitOp qualified as EmitOp
import Rule.LowComp.EmitValue
import Rule.LowType qualified as LT
import Rule.LowType.EmitLowType

type Label =
  Ident

-- per-function handle
data Handle = Handle
  { gensymHandle :: GensymN.Handle,
    emitOpHandle :: EmitOp.Handle,
    phiInfo :: Maybe (Ident, Label),
    currentLabel :: Maybe Label,
    retType :: Builder,
    labelMap :: IORef (IntMap.IntMap Ident)
  }

new :: Builder -> App Handle
new retTypeBuilder = do
  gensymHandle <- GensymN.new
  emitOpHandle <- toApp EmitOp.new
  emptyMapRef <- liftIO $ newIORef IntMap.empty
  return
    Handle
      { gensymHandle,
        emitOpHandle,
        phiInfo = Nothing,
        currentLabel = Nothing,
        retType = retTypeBuilder,
        labelMap = emptyMapRef
      }

emitLowComp :: Handle -> LC.Comp -> App [Builder]
emitLowComp h lowComp =
  case lowComp of
    LC.Return d ->
      case phiInfo h of
        Nothing ->
          return $ emitOp $ unwordsL ["ret", retType h, emitValue d]
        Just (phiSrcVar, rendezvous) -> do
          let lowOp = emitLowOp (emitOpHandle h) (emitValue (LC.VarLocal phiSrcVar) <> " = ") $ LC.Bitcast d LT.Pointer LT.Pointer
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
      ret <- emitLowComp h $ LC.Return (LC.VarLocal tmp)
      return $ op <> ret
    LC.Switch (d, lowType) defaultBranch branchList (phiTgt, cont) -> do
      defaultLabel <- Gensym.newIdentFromText "default"
      labelList <- liftIO $ constructLabelList h branchList
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
      case currentLabel h of
        Nothing ->
          return ()
        Just current -> do
          -- bypass switch clauses and get the confluence block
          liftIO $ modifyIORef' (labelMap h) $ IntMap.insert (toInt current) confluenceLabel
      phiSrcVarList <- mapM (const $ Gensym.newIdentFromText "phi") labelBranchList
      blockAsmList <-
        forM (zip labelBranchList phiSrcVarList) $ \((label, branch), phiSrcVar) -> do
          let newPhiInfo = Just (phiSrcVar, confluenceLabel)
          a <- emitLowComp (h {phiInfo = newPhiInfo, currentLabel = Just label}) branch
          return $ emitLabel ("_" <> intDec (toInt label)) : a
      let allLabelList = map fst labelBranchList
      currentLabelMap <- liftIO $ readIORef $ labelMap h
      let resolvedLabelList = resolveLabelList currentLabelMap allLabelList
      let phiOp =
            unwordsL
              [ "phi",
                emitLowType LT.Pointer,
                emitPhiList $ zip phiSrcVarList resolvedLabelList
              ]
      let phiOpStr = emitOp $ emitValue (LC.VarLocal phiTgt) <> " = " <> phiOp
      rendezvousBlock <- do
        a <- emitLowComp (h {currentLabel = Just confluenceLabel}) cont
        return $ emitLabel ("_" <> intDec (toInt confluenceLabel)) : phiOpStr <> a
      return $ switchOpStr <> concat blockAsmList <> rendezvousBlock
    LC.Cont op cont -> do
      let lowOp = emitLowOp (emitOpHandle h) "" op
      a <- emitLowComp h cont
      return $ lowOp <> a
    LC.Let x op cont -> do
      let lowOp = emitLowOp (emitOpHandle h) (emitValue (LC.VarLocal x) <> " = ") op
      a <- emitLowComp h cont
      return $ lowOp <> a
    LC.Unreachable -> do
      emitLowComp h $ LC.Return LC.Null

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

emitLowOp :: EmitOp.Handle -> Builder -> LC.Op -> [Builder]
emitLowOp ax prefix op = do
  emitOp $ prefix <> EmitOp.emitLowOp ax op

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

constructLabelList :: Handle -> [a] -> IO [Ident]
constructLabelList h input =
  case input of
    [] ->
      return []
    (_ : rest) -> do
      label <- GensymN.newIdentFromText (gensymHandle h) "case"
      labelList <- constructLabelList h rest
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
