module Entity.WeakTerm.ToText (toText, showDecisionTree, showGlobalVariable) where

import Control.Comonad.Cofree
import Data.Text qualified as T
import Entity.Attr.Data qualified as AttrD
import Entity.Attr.DataIntro qualified as AttrDI
import Entity.Attr.Lam qualified as AttrL
import Entity.Attr.VarGlobal qualified as AttrVG
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.PrimOp qualified as PO
import Entity.PrimType.ToText qualified as PT
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
    _ :< WT.VarGlobal _ x ->
      showGlobalVariable x
    _ :< WT.Pi xts cod -> do
      case xts of
        [(_, x, dom)]
          | isHole x ->
              toText dom <> " -> " <> toText cod
        _ ->
          inParen (showDomArgList xts) <> " -> " <> toText cod
    _ :< WT.PiIntro attr xts e -> do
      case attr of
        AttrL.Attr {lamKind = LK.Fix (_, x, _)} -> do
          "mu " <> showVariable x <> inParen (showDomArgList xts) <> " " <> inBrace (toText e)
        AttrL.Attr {lamKind = LK.Normal} -> do
          inParen (showDomArgList xts) <> " => " <> inBrace (toText e)
    _ :< WT.PiElim e es -> do
      case e of
        _ :< WT.VarGlobal attr _
          | AttrVG.isConstLike attr ->
              toText e
        _ -> do
          showApp (toText e) (map toText es)
    _ :< WT.Data (AttrD.Attr {..}) name es -> do
      if isConstLike
        then showGlobalVariable name
        else showApp (showGlobalVariable name) (map toText es)
    _ :< WT.DataIntro (AttrDI.Attr {..}) consName _ consArgs -> do
      if isConstLike
        then showGlobalVariable consName
        else showApp (showGlobalVariable consName) (map toText consArgs)
    _ :< WT.DataElim isNoetic xets tree -> do
      if isNoetic
        then "case " <> showMatchArgs xets <> " " <> inBrace (showDecisionTree tree)
        else "match " <> showMatchArgs xets <> " " <> inBrace (showDecisionTree tree)
    _ :< WT.Noema t ->
      "&" <> toText t
    _ :< WT.Embody _ e ->
      "*" <> toText e
    _ :< WT.Let opacity (_, x, t) e1 e2 -> do
      case opacity of
        WT.Noetic ->
          "tie " <> showVariable x <> ": " <> toText t <> " = " <> toText e1 <> " in " <> toText e2
        _ ->
          "let " <> showVariable x <> ": " <> toText t <> " = " <> toText e1 <> " in " <> toText e2
    _ :< WT.Prim prim ->
      showPrim prim
    _ :< WT.Hole {} ->
      "_"
    _ :< WT.ResourceType name ->
      showGlobalVariable name
    _ :< WT.Magic _ -> do
      "<magic>"
    _ :< WT.Annotation _ _ e ->
      toText e

inParen :: T.Text -> T.Text
inParen s =
  "(" <> s <> ")"

inBrace :: T.Text -> T.Text
inBrace s =
  "{ " <> s <> " }"

inBracket :: T.Text -> T.Text
inBracket s =
  "[" <> s <> "]"

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

showDomArg :: BinderF WT.WeakTerm -> T.Text
showDomArg (_, x, t) =
  showVariable x <> ": " <> toText t

showDomArgList :: [BinderF WT.WeakTerm] -> T.Text
showDomArgList mxts =
  T.intercalate ", " $ map showDomArg mxts

showApp :: T.Text -> [T.Text] -> T.Text
showApp e es =
  e <> inParen (T.intercalate ", " es)

showVariable :: Ident -> T.Text
showVariable x =
  if isHole x
    then "_"
    else Ident.toText x

showGlobalVariable :: DD.DefiniteDescription -> T.Text
showGlobalVariable =
  DD.localLocator

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
        WPV.Op op ->
          case op of
            PO.PrimUnaryOp name _ _ ->
              T.pack (show name)
            PO.PrimBinaryOp name _ _ ->
              T.pack (show name)
            PO.PrimCmpOp name _ _ ->
              T.pack (show name)
            PO.PrimConvOp name _ _ ->
              T.pack (show name)
        WPV.StaticText _ text ->
          T.pack $ show text

showMatchArgs :: [(Ident, WT.WeakTerm, WT.WeakTerm)] -> T.Text
showMatchArgs xets = do
  inParen $ T.intercalate ", " (map showMatchArg xets)

showMatchArg :: (Ident, WT.WeakTerm, WT.WeakTerm) -> T.Text
showMatchArg (x, e, t) = do
  showVariable x <> ": " <> toText t <> " = " <> toText e

showDecisionTree :: DT.DecisionTree WT.WeakTerm -> T.Text
showDecisionTree tree =
  case tree of
    DT.Leaf xs cont -> do
      showApp "leaf" [inBracket (T.intercalate ", " (map showVariable xs)), toText cont]
    DT.Unreachable ->
      "UNREACHABLE"
    DT.Switch (cursor, cursorType) (fallbackClause, clauseList) -> do
      "switch"
        <> inParen
          ( showVariable cursor
              <> ": "
              <> toText cursorType
          )
        <> inBrace (T.intercalate ", " (map showClauseList clauseList ++ [showDecisionTree fallbackClause]))

showClauseList :: DT.Case WT.WeakTerm -> T.Text
showClauseList decisionCase = do
  showApp
    (showGlobalVariable (DT.consDD decisionCase))
    [ T.pack (show (D.reify (DT.disc decisionCase))),
      inParen $ T.intercalate ", " $ map (\(e, t) -> toText e <> ": " <> toText t) (DT.dataArgs decisionCase),
      inParen $ showTypeArgs (DT.consArgs decisionCase),
      showDecisionTree (DT.cont decisionCase)
    ]
