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
import Data.List (transpose)
import Data.Maybe (fromMaybe)
import Gensym.Rule.Handle qualified as Gensym
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Rule.Handle.Global.Platform qualified as Platform
import Kernel.Emit.Rule.Builder
import Kernel.Emit.Rule.LowOp qualified as EmitOp
import Kernel.Emit.Rule.LowType
import Kernel.Emit.Rule.LowValue
import Language.Common.Move.CreateSymbol qualified as Gensym
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify
import Language.Common.Rule.LowType qualified as LT
import Language.LowComp.Rule.LowComp qualified as LC

type Label =
  Ident

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    emitOpHandle :: EmitOp.Handle,
    retType :: Builder,
    currentLabel :: Maybe Label,
    goalLabel :: Maybe Label,
    labelMapRef :: IORef (IntMap.IntMap Ident)
  }

new :: Global.Handle -> Builder -> IO Handle
new globalHandle retType = do
  let currentLabel = Nothing
  let goalLabel = Nothing
  labelMapRef <- liftIO $ newIORef IntMap.empty
  let baseSize = Platform.getDataSizeValue (Global.platformHandle globalHandle)
  let emitOpHandle = EmitOp.new baseSize
  let gensymHandle = Global.gensymHandle globalHandle
  return $ Handle {..}

emitLowComp :: Handle -> LC.Comp -> IO [Builder]
emitLowComp h lowComp =
  case lowComp of
    LC.Return d -> do
      return $ emitOp $ unwordsL ["ret", retType h, emitValue d]
    LC.Phi _ -> do
      case goalLabel h of
        Nothing -> do
          -- unreachable
          error $ "compiler bug: no goal label is found for the block `" <> show (currentLabel h) <> "`"
        Just goalLabel ->
          return $ emitOp $ unwordsL ["br", "label", emitValue (LC.VarLocal goalLabel)]
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
    LC.Switch (d, lowType) defaultBranch branchList (phiList, cont) -> do
      defaultLabel <- Gensym.newIdentFromText (gensymHandle h) "default"
      labelList <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "case") branchList
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
      let labelBranchList = (defaultLabel, defaultBranch) : zip labelList (map snd branchList)
      goalLabel <- Gensym.newIdentFromText (gensymHandle h) "goal"
      case currentLabel h of
        Nothing ->
          return ()
        Just current -> do
          liftIO $ modifyIORef' (labelMapRef h) $ IntMap.insert (toInt current) goalLabel
      blockAsmList <-
        forM labelBranchList $ \(label, branch) -> do
          a <- emitLowComp (h {currentLabel = Just label, goalLabel = Just goalLabel}) branch
          return $ emitLabel ("_" <> intDec (toInt label)) : a
      goalBlock <- do
        currentLabelMap <- liftIO $ readIORef $ labelMapRef h
        let (allLabelList, allBranchList) = unzip labelBranchList
        let resolvedLabelList = resolveLabelList currentLabelMap allLabelList
        let phiValueListList = transpose $ map (fromMaybe [] . LC.getPhiList) allBranchList
        let phiOpList = flip map (zip phiList phiValueListList) $ \(phiTgt, vs) -> do
              let phiOp = unwordsL ["phi", emitLowType LT.Pointer, emitPhiList $ zip vs resolvedLabelList]
              emitOp $ emitValue (LC.VarLocal phiTgt) <> " = " <> phiOp
        let phiOpStr = concat phiOpList
        a <- emitLowComp (h {currentLabel = Just goalLabel}) cont
        return $ emitLabel ("_" <> intDec (toInt goalLabel)) : phiOpStr <> a
      return $ switchOpStr <> concat blockAsmList <> goalBlock
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

emitPhiList :: [(LC.Value, Ident)] -> Builder
emitPhiList valueLabelList =
  case valueLabelList of
    [] ->
      ""
    [(value, label)] ->
      "[" <> emitValue value <> ", " <> emitValue (LC.VarLocal label) <> "]"
    (value, label) : rest ->
      showValueLabel value label <> ", " <> emitPhiList rest

showValueLabel :: LC.Value -> Ident -> Builder
showValueLabel value label =
  "[" <> emitValue value <> ", " <> emitValue (LC.VarLocal label) <> "]"

emitOp :: Builder -> [Builder]
emitOp s =
  ["  " <> s]

emitLabel :: Builder -> Builder
emitLabel s =
  s <> ":"

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
