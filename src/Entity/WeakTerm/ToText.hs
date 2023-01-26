module Entity.WeakTerm.ToText (toText, showDecisionTree) where

import Control.Comonad.Cofree
import Data.Text qualified as T
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.HoleID qualified as HID
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.PrimOp qualified as PO
import Entity.PrimType.ToText qualified as PT
import Entity.WeakArrayKind qualified as WAK
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT

toText :: WT.WeakTerm -> T.Text
toText term =
  case term of
    _ :< WT.Tau ->
      "tau"
    _ :< WT.Var x ->
      showVariable x
    _ :< WT.VarGlobal x _ ->
      DD.reify x
    _ :< WT.Pi xts cod
      | [(_, I ("internal.sigma-tau", _), _), (_, _, _ :< WT.Pi yts _)] <- xts ->
          case splitLast yts of
            Nothing ->
              "(product)"
            Just (zts, (_, _, t)) ->
              showCons ["∑", inParen $ showTypeArgs zts, toText t]
      | otherwise ->
          showCons ["Π", inParen $ showTypeArgs xts, toText cod]
    _ :< WT.PiIntro kind xts e -> do
      case kind of
        LK.Fix (_, x, _) -> do
          let argStr = inParen $ showItems $ map showArg xts
          showCons ["fix", showVariable x, argStr, toText e]
        _ -> do
          let argStr = inParen $ showItems $ map showArg xts
          showCons ["λ", argStr, toText e]
    _ :< WT.PiElim e es ->
      showCons $ map toText $ e : es
    _ :< WT.Data name es -> do
      showCons $ "{data}" <> DD.reify name : map toText es
    _ :< WT.DataIntro _ consName _ _ consArgs -> do
      showCons ("{data-intro}" <> DD.reify consName : map toText consArgs)
    _ :< WT.DataElim isNoetic xets tree -> do
      if isNoetic
        then showCons ["match-noetic", showMatchArgs xets, showDecisionTree tree]
        else showCons ["match", showMatchArgs xets, showDecisionTree tree]
    _ :< WT.Array ak ->
      case ak of
        WAK.PrimType t ->
          showCons ["array", toText t]
        WAK.General t ->
          showCons ["vector", toText t]
    _ :< WT.ArrayIntro _ es ->
      showCons $ "#arr" : map toText es
    _ :< WT.ArrayElim _ array index ->
      showCons ["access", toText array, toText index]
    _ :< WT.Noema t ->
      showCons ["noema", toText t]
    _ :< WT.Let _ (_, x, t) e1 e2 -> do
      showCons ["let", showVariable x, toText t, toText e1, toText e2]
    _ :< WT.Prim prim ->
      showPrim prim
    _ :< WT.Hole i es ->
      showCons $ "?M" <> T.pack (show (HID.reify i)) : map toText es
    _ :< WT.Magic m -> do
      let a = fmap toText m
      showCons [T.pack $ show a]

inParen :: T.Text -> T.Text
inParen s =
  "(" <> s <> ")"

showArg :: (Hint, Ident, WT.WeakTerm) -> T.Text
showArg (_, x, t) =
  inParen $ showVariable x <> " " <> toText t

showTypeArgs :: [BinderF WT.WeakTerm] -> T.Text
showTypeArgs args =
  case args of
    [] ->
      T.empty
    [(_, x, t)] ->
      inParen $ showVariable x <> " " <> toText t
    (_, x, t) : xts -> do
      let s1 = inParen $ showVariable x <> " " <> toText t
      let s2 = showTypeArgs xts
      s1 <> " " <> s2

showVariable :: Ident -> T.Text
showVariable =
  Ident.toText'

showItems :: [T.Text] -> T.Text
showItems =
  T.intercalate " "

showPrim :: WP.WeakPrim WT.WeakTerm -> T.Text
showPrim prim =
  case prim of
    WP.Type t ->
      PT.toText t
    WP.Value primValue ->
      case primValue of
        WPV.Int _ v ->
          T.pack (show v)
        WPV.Float _ v ->
          T.pack (show v)
        WPV.Op (PO.PrimOp opName _ _) ->
          opName

showCons :: [T.Text] -> T.Text
showCons =
  inParen . T.intercalate " "

splitLast :: [a] -> Maybe ([a], a)
splitLast xs =
  if null xs
    then Nothing
    else Just (init xs, last xs)

showMatchArgs :: [(Ident, WT.WeakTerm, WT.WeakTerm)] -> T.Text
showMatchArgs xets = do
  showCons $ map showMatchArg xets

showMatchArg :: (Ident, WT.WeakTerm, WT.WeakTerm) -> T.Text
showMatchArg (x, e, t) = do
  showCons [showVariable x, toText e, toText t]

showDecisionTree :: DT.DecisionTree WT.WeakTerm -> T.Text
showDecisionTree tree =
  case tree of
    DT.Leaf xs cont ->
      showCons ["leaf", showCons (map showVariable xs), toText cont]
    DT.Unreachable ->
      "UNREACHABLE"
    DT.Switch (cursor, cursorType) (fallbackClause, clauseList) -> do
      showCons $
        "switch"
          : showCons [showCons [showVariable cursor, toText cursorType]]
          : showDecisionTree fallbackClause
          : map showClauseList clauseList

showClauseList :: DT.Case WT.WeakTerm -> T.Text
showClauseList (DT.Cons consName d dataArgs consArgs cont) = do
  showCons
    [ DD.reify consName,
      T.pack (show (D.reify d)),
      showCons $ map (\(e, t) -> showCons [toText e, toText t]) dataArgs,
      inParen $ showTypeArgs consArgs,
      showDecisionTree cont
    ]
