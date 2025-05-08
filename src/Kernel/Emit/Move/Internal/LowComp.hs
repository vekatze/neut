module Kernel.Emit.Move.Internal.LowComp
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
import Gensym.Rule.Handle qualified as Gensym
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Rule.Handle.Global.Platform qualified as Platform
import Kernel.Emit.Rule.Builder
import Kernel.Emit.Rule.LowType
import Kernel.Emit.Rule.LowValue
import Language.Common.Move.CreateSymbol qualified as Gensym
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify
import Language.Common.Rule.LowType qualified as LT
import Language.LowComp.Rule.LowComp qualified as LC
import Language.LowComp.Rule.LowComp.EmitOp qualified as EmitOp

type Label =
  Ident

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    emitOpHandle :: EmitOp.Handle,
    retType :: Builder,
    phiInfo :: Maybe (Ident, Label),
    currentLabel :: Maybe Label,
    labelMapRef :: IORef (IntMap.IntMap Ident)
  }

new :: Global.Handle -> Builder -> IO Handle
new globalHandle retType = do
  let phiInfo = Nothing
  let currentLabel = Nothing
  labelMapRef <- liftIO $ newIORef IntMap.empty
  let baseSize = Platform.getDataSizeValue (Global.platformHandle globalHandle)
  let emitOpHandle = EmitOp.new baseSize
  let gensymHandle = Global.gensymHandle globalHandle
  return $ Handle {..}

emitLowComp :: Handle -> LC.Comp -> IO [Builder]
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
      tmp <- Gensym.newIdentFromText (gensymHandle h) "tmp"
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
      defaultLabel <- Gensym.newIdentFromText (gensymHandle h) "default"
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
      confluenceLabel <- Gensym.newIdentFromText (gensymHandle h) "confluence"
      case currentLabel h of
        Nothing ->
          return ()
        Just current -> do
          -- bypass switch clauses and get the confluence block
          liftIO $ modifyIORef' (labelMapRef h) $ IntMap.insert (toInt current) confluenceLabel
      phiSrcVarList <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "phi") labelBranchList
      blockAsmList <-
        forM (zip labelBranchList phiSrcVarList) $ \((label, branch), phiSrcVar) -> do
          let newPhiInfo = Just (phiSrcVar, confluenceLabel)
          a <- emitLowComp (h {phiInfo = newPhiInfo, currentLabel = Just label}) branch
          return $ emitLabel ("_" <> intDec (toInt label)) : a
      let allLabelList = map fst labelBranchList
      currentLabelMap <- liftIO $ readIORef $ labelMapRef h
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
resolveLabelList labelMapRef xs =
  case xs of
    [] ->
      []
    x : rest ->
      case IntMap.lookup (toInt x) labelMapRef of
        Just y ->
          resolveLabelList labelMapRef $ y : rest
        Nothing ->
          x : resolveLabelList labelMapRef rest

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
      label <- Gensym.newIdentFromText (gensymHandle h) "case"
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
