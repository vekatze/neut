module Language.WeakTerm.Rule.WeakTerm.ToText (toText) where

import Control.Comonad.Cofree
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Language.Common.Rule.Attr.Data qualified as AttrD
import Language.Common.Rule.Attr.DataIntro qualified as AttrDI
import Language.Common.Rule.Attr.Lam qualified as AttrL
import Language.Common.Rule.Attr.VarGlobal qualified as AttrVG
import Language.Common.Rule.Binder
import Language.Common.Rule.DecisionTree qualified as DT
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Discriminant qualified as D
import Language.Common.Rule.HoleID qualified as HID
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify qualified as Ident
import Language.Common.Rule.ImpArgs qualified as ImpArgs
import Language.Common.Rule.LamKind qualified as LK
import Language.Common.Rule.PiKind qualified as PK
import Language.Common.Rule.PrimOp qualified as PO
import Language.Common.Rule.PrimType.ToText qualified as PT
import Language.Common.Rule.Rune qualified as RU
import Language.WeakTerm.Rule.WeakPrim qualified as WP
import Language.WeakTerm.Rule.WeakPrimValue qualified as WPV
import Language.WeakTerm.Rule.WeakTerm qualified as WT

toText :: WT.WeakTerm -> T.Text
toText term =
  case term of
    _ :< WT.Tau ->
      "type"
    _ :< WT.Var x ->
      showVariable x
    _ :< WT.VarGlobal _ x ->
      showGlobalVariable x
    _ :< WT.Pi piKind impArgs expArgs cod -> do
      case piKind of
        PK.Normal isConstLike ->
          if isConstLike
            then showImpArgs impArgs <> " " <> toText cod
            else showImpArgs impArgs <> inParen (showDomArgList expArgs) <> " -> " <> toText cod
        PK.DataIntro _ -> do
          showDataImpArgs impArgs <> toText cod
    _ :< WT.PiIntro attr impArgs expArgs e -> do
      case attr of
        AttrL.Attr {lamKind = LK.Fix (_, x, codType)} ->
          "define "
            <> showVariable x
            <> showImpArgs impArgs
            <> inParen (showDomArgList expArgs)
            <> ": "
            <> toText codType
            <> " "
            <> inBrace (toText e)
        AttrL.Attr {lamKind = LK.Normal mName codType} -> do
          let name = fromMaybe "" mName
          "function "
            <> name
            <> showImpArgs impArgs
            <> inParen (showDomArgList expArgs)
            <> ": "
            <> toText codType
            <> " "
            <> inBrace (toText e)
    _ :< WT.PiElim _ e impArgs expArgs -> do
      case e of
        _ :< WT.VarGlobal attr _
          | AttrVG.isConstLike attr ->
              toText e
        _ -> do
          case impArgs of
            ImpArgs.FullySpecified impArgs' ->
              showApp' (toText e) (map toText impArgs') (map toText expArgs)
            ImpArgs.Unspecified ->
              showApp (toText e) (map toText expArgs)
            ImpArgs.PartiallySpecified impArgs' ->
              showApp' (toText e) (map toText (ImpArgs.extract (ImpArgs.PartiallySpecified impArgs'))) (map toText expArgs)
    _ :< WT.PiElimExact e -> do
      "exact " <> toText e
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
    _ :< WT.Box t ->
      "meta " <> toText t
    _ :< WT.BoxNoema t ->
      "&" <> toText t
    _ :< WT.BoxIntro letSeq t -> do
      let ks = map (\((_, x, _), _) -> x) letSeq
      "box " <> T.intercalate ", " (map Ident.toText ks) <> inBrace (toText t)
    _ :< WT.BoxIntroQuote e ->
      "quote " <> inBrace (toText e)
    _ :< WT.BoxElim castSeq (_, x, t) e1 _ e2 -> do
      let ks = map (\((_, y, _), _) -> y) castSeq
      let ks' = if null ks then "" else "on " <> T.intercalate ", " (map Ident.toText ks)
      "letbox "
        <> showVariable x
        <> ": "
        <> toText t
        <> ks'
        <> " = "
        <> toText e1
        <> " in "
        <> toText e2
    _ :< WT.Actual e ->
      "ACTUAL(" <> toText e <> ")"
    _ :< WT.Let opacity (_, x, t) e1 e2 -> do
      case opacity of
        WT.Noetic ->
          "tie " <> showVariable x <> ": " <> toText t <> " = " <> toText e1 <> " in " <> toText e2
        _ ->
          "let " <> showVariable x <> ": " <> toText t <> " = " <> toText e1 <> " in " <> toText e2
    _ :< WT.Prim prim ->
      showPrim prim
    _ :< WT.Hole i es ->
      "?" <> T.pack (show (HID.reify i)) <> "(" <> T.intercalate "," (map toText es) <> ")"
    _ :< WT.Magic _ -> do
      "<magic>"
    _ :< WT.Annotation _ _ e ->
      toText e
    _ :< WT.Resource dd _ _ _ _ _ -> do
      showGlobalVariable dd
    _ :< WT.Void ->
      "void"

showImpArgs :: [(BinderF WT.WeakTerm, Maybe WT.WeakTerm)] -> T.Text
showImpArgs impArgs =
  if null impArgs
    then ""
    else inAngleBracket $ showImpDomArgList impArgs

showDataImpArgs :: [(BinderF WT.WeakTerm, Maybe WT.WeakTerm)] -> T.Text
showDataImpArgs impArgs =
  if null impArgs
    then ""
    else "∀ " <> T.intercalate " " (map showDataImpArgWithDefault impArgs) <> ". "

showImpDomArgList :: [(BinderF WT.WeakTerm, Maybe WT.WeakTerm)] -> T.Text
showImpDomArgList mxts =
  T.intercalate ", " $ map showImpDomArgWithDefault mxts

showImpDomArgWithDefault :: (BinderF WT.WeakTerm, Maybe WT.WeakTerm) -> T.Text
showImpDomArgWithDefault ((_, x, _), maybeDefault) = do
  let baseArg = showVariable x
  case maybeDefault of
    Nothing ->
      baseArg
    Just defaultValue ->
      baseArg <> " := " <> toText defaultValue

showDataImpArgWithDefault :: (BinderF WT.WeakTerm, Maybe WT.WeakTerm) -> T.Text
showDataImpArgWithDefault ((_, x, t), maybeDefault) = do
  let baseArg =
        case t of
          _ :< WT.Tau -> showVariable x
          _ -> "(" <> showVariable x <> ": " <> toText t <> ")"
  case maybeDefault of
    Nothing -> baseArg
    Just defaultValue -> "(" <> baseArg <> " := " <> toText defaultValue <> ")"

inParen :: T.Text -> T.Text
inParen s =
  "(" <> s <> ")"

inBrace :: T.Text -> T.Text
inBrace s =
  "{ " <> s <> " }"

inBracket :: T.Text -> T.Text
inBracket s =
  "[" <> s <> "]"

inAngleBracket :: T.Text -> T.Text
inAngleBracket s =
  "<" <> s <> ">"

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

showApp' :: T.Text -> [T.Text] -> [T.Text] -> T.Text
showApp' e impArgs expArgs =
  e <> inAngleBracket (T.intercalate ", " impArgs) <> inParen (T.intercalate ", " expArgs)

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
        WPV.Rune r ->
          "`" <> T.replace "`" "\\`" (RU.asText r) <> "`"

showMatchArgs :: [(Ident, WT.WeakTerm, WT.WeakTerm)] -> T.Text
showMatchArgs xets = do
  inParen $ T.intercalate ", " (map showMatchArg xets)

showMatchArg :: (Ident, WT.WeakTerm, WT.WeakTerm) -> T.Text
showMatchArg (x, e, t) = do
  showVariable x <> ": " <> toText t <> " = " <> toText e

showLeafLetItem :: (BinderF WT.WeakTerm, WT.WeakTerm) -> T.Text
showLeafLetItem ((_, x, t), e) =
  showVariable x <> ": " <> toText t <> " = " <> toText e

showDecisionTree :: DT.DecisionTree WT.WeakTerm -> T.Text
showDecisionTree tree =
  case tree of
    DT.Leaf xs letSeq cont -> do
      showApp
        "leaf"
        [ inBracket (T.intercalate ", " (map showVariable xs)),
          inParen $ T.intercalate ", " (map showLeafLetItem letSeq),
          toText cont
        ]
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
  case decisionCase of
    DT.LiteralCase _ i cont -> do
      showApp "literal" [T.pack (show i), showDecisionTree cont]
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      showApp
        (showGlobalVariable consDD)
        [ T.pack (show (D.reify disc)),
          inParen $ T.intercalate ", " $ map (\(e, t) -> toText e <> ": " <> toText t) dataArgs,
          inParen $ showTypeArgs consArgs,
          showDecisionTree cont
        ]
