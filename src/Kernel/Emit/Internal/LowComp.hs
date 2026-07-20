module Kernel.Emit.Internal.LowComp
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
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Gensym.Handle qualified as Gensym
import Kernel.Common.Allocator (AllocatorSpec)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Emit.Builder
import Kernel.Emit.LowOp qualified as EmitOp
import Kernel.Emit.LowType
import Kernel.Emit.LowValue
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.Ident
import Language.Common.Ident.Reify
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.LowType qualified as LT
import Language.LowComp.LowComp qualified as LC

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

new :: Gensym.Handle -> Global.Handle -> Builder -> AllocatorSpec -> IO Handle
new gensymHandle globalHandle retType allocatorSpec = do
  let currentLabel = Nothing
  let goalLabel = Nothing
  labelMapRef <- liftIO $ newIORef IntMap.empty
  let baseSize = Platform.getDataSize (Global.platformHandle globalHandle)
  let emitOpHandle = EmitOp.new baseSize allocatorSpec
  return $ Handle {..}

emitLowComp :: Handle -> LC.Comp -> IO [Builder]
emitLowComp h lowComp =
  case lowComp of
    LC.Return d -> do
      case goalLabel h of
        Nothing ->
          return $ emitOp $ unwordsL ["ret", retType h, emitValue d]
        Just joinLabel ->
          return $ emitOp $ unwordsL ["br", "label", emitIdentAsLabelVar joinLabel]
    LC.ReturnVoid -> do
      return $ emitOp "ret void"
    LC.Phi _ -> do
      case goalLabel h of
        Nothing ->
          error $ "compiler bug: no goal label is found for the block `" <> show (currentLabel h) <> "`"
        Just joinLabel ->
          return $ emitOp $ unwordsL ["br", "label", emitIdentAsLabelVar joinLabel]
    LC.TailCall codType f args -> do
      case codType of
        LT.Void -> do
          let op =
                emitOp $
                  unwordsL
                    [ "tail call fastcc",
                      emitInternalReturnType codType,
                      emitValue f <> showInternalArgs args
                    ]
          ret <- emitLowComp (h {goalLabel = Nothing}) LC.ReturnVoid
          return $ op <> ret
        _ -> do
          tmp <- Gensym.newIdentFromText (gensymHandle h) "tmp"
          let op =
                emitOp $
                  unwordsL
                    [ emitValue (LC.VarLocal tmp),
                      "=",
                      "tail call fastcc",
                      emitInternalReturnType codType,
                      emitValue f <> showInternalArgs args
                    ]
          ret <- emitLowComp (h {goalLabel = Nothing}) $ LC.Return (LC.VarLocal tmp)
          return $ op <> ret
    LC.Switch d lowType defaultBranch branchList phiTargets cont -> do
      switchID <- Gensym.newIdentFromText (gensymHandle h) "switch"
      labelList <- mapM (newSwitchCaseLabel (gensymHandle h) switchID) [0 .. length branchList - 1]
      defaultLabel <- newSwitchSpecialLabel (gensymHandle h) switchID "d"
      let switchOpStr =
            emitOp $
              unwordsL
                [ "switch",
                  emitLowType lowType,
                  emitValue d <> ",",
                  "label",
                  emitIdentAsLabelVar defaultLabel,
                  showBranchList lowType $ zip (map fst branchList) labelList
                ]
      let labelBranchList = zip labelList (map snd branchList) ++ [(defaultLabel, defaultBranch)]
      goalLabel <- newSwitchSpecialLabel (gensymHandle h) switchID "g"
      case currentLabel h of
        Nothing ->
          return ()
        Just current -> do
          liftIO $ modifyIORef' (labelMapRef h) $ IntMap.insert (toInt current) goalLabel
      blockAsmList <-
        forM labelBranchList $ \(label, branch) -> do
          a <- emitLowComp (h {currentLabel = Just label, goalLabel = Just goalLabel}) branch
          return $ emitLabel (emitIdentAsLabel label) : a
      goalBlock <- do
        currentLabelMap <- liftIO $ readIORef $ labelMapRef h
        let (allLabelList, allBranchList) = unzip labelBranchList
        let resolvedLabelList = resolveLabelList currentLabelMap allLabelList
        let phiBranchList =
              flip mapMaybe (zip resolvedLabelList allBranchList) $ \(label, branch) -> do
                values <- LC.getPhiList branch
                return (label, values)
        let (reachableLabels, phiValueLists) = unzip phiBranchList
        let phiValueListList = transpose phiValueLists
        let phiOpList =
              flip map (zip phiTargets phiValueListList) $ \(phiTarget, values) -> do
                let phiOp = unwordsL ["phi", emitLowType LT.Pointer, emitPhiList $ zip values reachableLabels]
                emitOp $ emitValue (LC.VarLocal phiTarget) <> " = " <> phiOp
        if null phiBranchList && not (null phiTargets)
          then return $ emitLabel (emitIdentAsLabel goalLabel) : emitOp "unreachable"
          else do
            let phiOpStr = concat phiOpList
            a <- emitLowComp (h {currentLabel = Just goalLabel}) cont
            return $ emitLabel (emitIdentAsLabel goalLabel) : phiOpStr <> a
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
      return $ emitOp "unreachable"

newSwitchCaseLabel :: Gensym.Handle -> Ident -> Int -> IO Ident
newSwitchCaseLabel gensymHandle switchID caseIndex = do
  Gensym.newIdentFromText gensymHandle $ switchLabelPrefix switchID <> "c" <> T.pack (show caseIndex)

newSwitchSpecialLabel :: Gensym.Handle -> Ident -> T.Text -> IO Ident
newSwitchSpecialLabel gensymHandle switchID suffix = do
  Gensym.newIdentFromText gensymHandle $ switchLabelPrefix switchID <> suffix

switchLabelPrefix :: Ident -> T.Text
switchLabelPrefix switchID =
  "switch-label:s" <> T.pack (show (Ident.toInt switchID))

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
emitLowOp ax prefix op =
  case op of
    LC.StackLifetimeStart {} ->
      []
    LC.StackLifetimeEnd {} ->
      []
    _ ->
      emitOp $ prefix <> EmitOp.emitLowOp ax op

emitPhiList :: [(LC.Value, Ident)] -> Builder
emitPhiList valueLabelList =
  case valueLabelList of
    [] ->
      ""
    [(value, label)] ->
      "[" <> emitValue value <> ", " <> emitIdentAsLabelVar label <> "]"
    (value, label) : rest ->
      showValueLabel value label <> ", " <> emitPhiList rest

showValueLabel :: LC.Value -> Ident -> Builder
showValueLabel value label =
  "[" <> emitValue value <> ", " <> emitIdentAsLabelVar label <> "]"

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
    <> emitIdentAsLabelVar label
