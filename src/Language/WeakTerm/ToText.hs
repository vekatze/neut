module Language.WeakTerm.ToText (toText, toTextType) where

import Control.Comonad.Cofree
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Attr.VarGlobal qualified as AttrVG
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.HoleID qualified as HID
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.LamKind qualified as LK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.Opacity qualified as O
import Language.Common.PiKind qualified as PK
import Language.Common.PrimOp qualified as PO
import Language.Common.PrimType.ToText qualified as PT
import Language.Common.Rune qualified as RU
import Language.Common.VarKind qualified as VK
import Language.WeakTerm.WeakPrimValue qualified as WPV
import Language.WeakTerm.WeakTerm qualified as WT

toText :: WT.WeakTerm -> T.Text
toText term =
  case term of
    _ :< WT.Var x ->
      showVariable x
    _ :< WT.VarGlobal _ x ->
      showGlobalVariable x
    _ :< WT.PiIntro attr impArgs expArgs defaultArgs e -> do
      case attr of
        AttrL.Attr {lamKind = LK.Fix opacity isDestPassing (_, k, x, codType)} ->
          ( if isDestPassing
              then "define-dest-passing "
              else case opacity of
                O.Opaque -> "define "
                O.Clear -> "inline "
          )
            <> showVarWithKind k x
            <> showImpArgs impArgs []
            <> inParen (showFnDomArgList expArgs)
            <> showDefaultArgs defaultArgs
            <> ": "
            <> toTextType codType
            <> " "
            <> inBrace (toText e)
        AttrL.Attr {lamKind = LK.Normal mName isDestPassing codType} -> do
          let name = fromMaybe "" mName
          (if isDestPassing then "function-dest-passing " else "function ")
            <> name
            <> showImpArgs impArgs []
            <> inParen (showFnDomArgList expArgs)
            <> showDefaultArgs defaultArgs
            <> ": "
            <> toTextType codType
            <> " "
            <> inBrace (toText e)
    _ :< WT.PiElim _ e impArgs expArgs _ -> do
      let impArgsText = case impArgs of
            ImpArgs.Unspecified -> ""
            ImpArgs.FullySpecified ts -> inAngleBracket (T.intercalate ", " (map toTextType ts))
      case e of
        _ :< WT.VarGlobal attr _
          | AttrVG.isConstLike attr ->
              toText e <> impArgsText
        _ ->
          toText e <> impArgsText <> inParen (T.intercalate ", " (map toText expArgs))
    _ :< WT.PiElimExact e -> do
      "exact " <> toText e
    _ :< WT.DataIntro (AttrDI.Attr {..}) consName _ consArgs -> do
      if isConstLike
        then showGlobalVariable consName
        else showApp (showGlobalVariable consName) (map toText consArgs)
    _ :< WT.DataElim isNoetic xets tree -> do
      if isNoetic
        then "case " <> showMatchArgs xets <> " " <> inBrace (showDecisionTree tree)
        else "match " <> showMatchArgs xets <> " " <> inBrace (showDecisionTree tree)
    _ :< WT.BoxIntro letSeq t -> do
      let kes = map (\((_, _, x, _), e) -> (x, e)) letSeq
      "box " <> T.intercalate ", " (map (\(k, e) -> inParen $ Ident.toText k <> ", " <> toText e) kes) <> inBrace (toText t)
    _ :< WT.BoxIntroLift e ->
      "lift " <> inBrace (toText e)
    _ :< WT.BoxElim castSeq (_, k, x, t) e1 _ e2 -> do
      let ks = map (\((_, _, y, _), _) -> y) castSeq
      let ks' = if null ks then "" else " on " <> T.intercalate ", " (map Ident.toText ks)
      "letbox "
        <> showVarWithKind k x
        <> ": "
        <> toTextType t
        <> ks'
        <> " = "
        <> toText e1
        <> "; "
        <> toText e2
    _ :< WT.CodeIntro e ->
      "quote " <> inBrace (toText e)
    _ :< WT.CodeElim e ->
      "unquote " <> inBrace (toText e)
    _ :< WT.TauIntro ty ->
      "pack-type" <> inParen (toTextType ty)
    _ :< WT.TauElim (_, x) e1 e2 ->
      "unpack-type " <> showVariable x <> " = " <> toText e1 <> "; " <> toText e2
    _ :< WT.Actual e ->
      "ACTUAL(" <> toText e <> ")"
    _ :< WT.Let opacity (_, k, x, t) e1 e2 -> do
      case opacity of
        WT.Noetic ->
          "tie " <> showVarWithKind k x <> ": " <> toTextType t <> " = " <> toText e1 <> "; " <> toText e2
        _ ->
          "let " <> showVarWithKind k x <> ": " <> toTextType t <> " = " <> toText e1 <> "; " <> toText e2
    _ :< WT.Prim primValue ->
      showPrimValue primValue
    _ :< WT.Magic magic -> do
      showMagic magic
    _ :< WT.Annotation _ _ e ->
      toText e

toTextType :: WT.WeakType -> T.Text
toTextType ty =
  case ty of
    _ :< WT.Tau ->
      "type"
    _ :< WT.TVar x ->
      showVariable x
    _ :< WT.TVarGlobal _ x ->
      showGlobalVariable x
    _ :< WT.TyApp t args -> do
      case t of
        _ :< WT.TVarGlobal attr _
          | AttrVG.isConstLike attr ->
              toTextType t
        _ ->
          showApp (toTextType t) (map toTextType args)
    _ :< WT.Pi piKind impArgs expArgs defaultArgs cod -> do
      case piKind of
        PK.Normal isConstLike ->
          if isConstLike
            then showImpArgsForAll impArgs defaultArgs <> toTextType cod
            else showImpArgs impArgs [] <> inParen (showDomArgList expArgs) <> showDefaultBinders defaultArgs <> " -> " <> toTextType cod
        PK.DestPass isConstLike ->
          if isConstLike
            then showImpArgsForAll impArgs defaultArgs <> toTextType cod
            else showImpArgs impArgs [] <> inParen (showDomArgList expArgs) <> showDefaultBinders defaultArgs <> " ->> " <> toTextType cod
        PK.DataIntro _ -> do
          showImpArgsForAll impArgs defaultArgs <> toTextType cod
    _ :< WT.Data (AttrD.Attr {..}) name es -> do
      if isConstLike
        then showGlobalVariable name
        else showApp (showGlobalVariable name) (map toTextType es)
    _ :< WT.Box t ->
      "+" <> toTextType t
    _ :< WT.BoxNoema t ->
      "&" <> toTextType t
    _ :< WT.Code t ->
      "'" <> toTextType t
    _ :< WT.PrimType pt ->
      PT.toText pt
    _ :< WT.Void ->
      "void"
    _ :< WT.Resource dd _ -> do
      showGlobalVariable dd
    _ :< WT.TypeHole i es ->
      "?" <> T.pack (show (HID.reify i)) <> "(" <> T.intercalate "," (map toTextType es) <> ")"

showImpArgs :: [BinderF WT.WeakType] -> [(BinderF WT.WeakType, WT.WeakTerm)] -> T.Text
showImpArgs impArgs defaultArgs = do
  let nonDefaultDoc =
        if null impArgs
          then ""
          else inAngleBracket $ showImpDomArgList impArgs
  let defaultDoc =
        if null defaultArgs
          then ""
          else inBracket $ showDefaultDomArgList defaultArgs
  nonDefaultDoc <> defaultDoc

showDefaultArgs :: [(BinderF WT.WeakType, WT.WeakTerm)] -> T.Text
showDefaultArgs defaultArgs =
  if null defaultArgs
    then ""
    else inBracket $ showDefaultDomArgList defaultArgs

showImpArgsForAll :: [BinderF WT.WeakType] -> [BinderF WT.WeakType] -> T.Text
showImpArgsForAll impArgs defaultArgs = do
  let impArgsWithDefaults = map (,Nothing) impArgs ++ map (,Nothing) defaultArgs
  if null impArgsWithDefaults
    then ""
    else "∀ " <> T.intercalate " " (map showDataImpArgWithDefault impArgsWithDefaults) <> ". "

showDefaultBinders :: [BinderF WT.WeakType] -> T.Text
showDefaultBinders defaultArgs =
  if null defaultArgs
    then ""
    else inBracket $ showDefaultDomBinderList defaultArgs

showImpDomArgList :: [BinderF WT.WeakType] -> T.Text
showImpDomArgList mxts =
  T.intercalate ", " $ map showImpDomArg mxts

showDefaultDomArgList :: [(BinderF WT.WeakType, WT.WeakTerm)] -> T.Text
showDefaultDomArgList mxts =
  T.intercalate ", " $ map showDefaultDomArg mxts

showDefaultDomBinderList :: [BinderF WT.WeakType] -> T.Text
showDefaultDomBinderList mxts =
  T.intercalate ", " $ map showDefaultDomBinder mxts

showImpDomArg :: BinderF WT.WeakType -> T.Text
showImpDomArg (_, k, x, _) =
  showVarWithKind k x

showDefaultDomArg :: (BinderF WT.WeakType, WT.WeakTerm) -> T.Text
showDefaultDomArg ((_, k, x, t), defaultValue) =
  showVarWithKind k x <> ": " <> toTextType t <> " := " <> toText defaultValue

showDefaultDomBinder :: BinderF WT.WeakType -> T.Text
showDefaultDomBinder (_, k, x, t) =
  showVarWithKind k x <> ": " <> toTextType t

showDataImpArgWithDefault :: (BinderF WT.WeakType, Maybe WT.WeakTerm) -> T.Text
showDataImpArgWithDefault ((_, k, x, t), maybeDefault) = do
  let baseArg =
        case t of
          _ :< WT.Tau -> showVarWithKind k x
          _ -> "(" <> showVarWithKind k x <> ": " <> toTextType t <> ")"
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

showTypeArgs :: [BinderF WT.WeakType] -> T.Text
showTypeArgs args =
  case args of
    [] ->
      T.empty
    [(_, k, x, t)] ->
      inParen $ showVarWithKind k x <> " " <> toTextType t
    (_, k, x, t) : xts -> do
      let s1 = inParen $ showVarWithKind k x <> " " <> toTextType t
      let s2 = showTypeArgs xts
      s1 <> " " <> s2

showDomArg :: BinderF WT.WeakType -> T.Text
showDomArg (_, _, _, t) =
  toTextType t

showFnDomArg :: BinderF WT.WeakType -> T.Text
showFnDomArg (_, k, x, t) =
  showVarWithKind k x <> ": " <> toTextType t

showVarWithKind :: VK.VarKind -> Ident -> T.Text
showVarWithKind k x =
  case k of
    VK.Exp -> "!" <> showVariable x
    VK.Normal -> showVariable x

showDomArgList :: [BinderF WT.WeakType] -> T.Text
showDomArgList mxts =
  T.intercalate ", " $ map showDomArg mxts

showFnDomArgList :: [BinderF WT.WeakType] -> T.Text
showFnDomArgList mxts =
  T.intercalate ", " $ map showFnDomArg mxts

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

showPrimValue :: WPV.WeakPrimValue WT.WeakType -> T.Text
showPrimValue primValue =
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

showMatchArgs :: [(Ident, WT.WeakTerm, WT.WeakType)] -> T.Text
showMatchArgs xets = do
  inParen $ T.intercalate ", " (map showMatchArg xets)

showMatchArg :: (Ident, WT.WeakTerm, WT.WeakType) -> T.Text
showMatchArg (x, e, t) = do
  showVariable x <> ": " <> toTextType t <> " = " <> toText e

showLeafLetItem :: (BinderF WT.WeakType, WT.WeakTerm) -> T.Text
showLeafLetItem ((_, k, x, t), e) =
  showVarWithKind k x <> ": " <> toTextType t <> " = " <> toText e

showDecisionTree :: DT.DecisionTree WT.WeakType WT.WeakTerm -> T.Text
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
              <> toTextType cursorType
          )
        <> inBrace (T.intercalate ", " (map showClauseList clauseList ++ [showDecisionTree fallbackClause]))

showClauseList :: DT.Case WT.WeakType WT.WeakTerm -> T.Text
showClauseList decisionCase = do
  case decisionCase of
    DT.LiteralCase _ i cont -> do
      showApp "literal" [T.pack (show i), showDecisionTree cont]
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      showApp
        (showGlobalVariable consDD)
        [ T.pack (show (D.reify disc)),
          inParen $ T.intercalate ", " $ map (\(e, t) -> toTextType e <> ": " <> toTextType t) dataArgs,
          inParen $ showTypeArgs consArgs,
          showDecisionTree cont
        ]

showMagic :: M.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm -> T.Text
showMagic (M.WeakMagic magic) =
  case magic of
    M.LowMagic lowMagic ->
      showLowMagic lowMagic
    M.Calloc sizeType num size ->
      "magic calloc" <> inParen (toTextType sizeType <> ", " <> toText num <> ", " <> toText size)
    M.Malloc sizeType size ->
      "magic malloc" <> inParen (toTextType sizeType <> ", " <> toText size)
    M.Realloc sizeType ptr size ->
      "magic realloc" <> inParen (toTextType sizeType <> ", " <> toText ptr <> ", " <> toText size)
    M.Free _ ptr ->
      "magic free" <> inParen (toText ptr)
    M.InspectType _ typeValueExpr e ->
      "magic inspect-type" <> inParen (toTextType typeValueExpr <> ", " <> toTextType e)
    M.EqType _ typeExpr1 typeExpr2 ->
      "magic eq-type" <> inParen (toTextType typeExpr1 <> ", " <> toTextType typeExpr2)
    M.ShowType stringTypeExpr typeExpr ->
      "magic show-type" <> inParen (toTextType stringTypeExpr <> ", " <> toTextType typeExpr)
    M.StringCons stringTypeExpr rune text ->
      "magic text-cons" <> inParen (toTextType stringTypeExpr <> ", " <> toText rune <> ", " <> toText text)
    M.StringUncons _ text ->
      "magic text-uncons" <> inParen (toText text)
    M.CompileError typeExpr msg ->
      "magic compile-error" <> inParen (toTextType typeExpr <> ", " <> toText msg)

showLowMagic :: LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm -> T.Text
showLowMagic lowMagic =
  case lowMagic of
    LM.Cast from to value ->
      "magic cast" <> inParen (toTextType from <> ", " <> toTextType to <> ", " <> toText value)
    LM.Store t unit value pointer ->
      "magic store"
        <> inParen
          ( toTextType t
              <> ", "
              <> toTextType unit
              <> ", "
              <> toText value
              <> ", "
              <> toText pointer
          )
    LM.Load t pointer ->
      "magic load" <> inParen (toTextType t <> ", " <> toText pointer)
    LM.Alloca t size ->
      "magic alloca" <> inParen (toTextType t <> ", " <> toText size)
    LM.External domList cod extFunName args varArgs ->
      let domStr = T.intercalate ", " (map toTextType domList)
          codStr = showForeignCodType cod
          argsStr = T.intercalate ", " (map toText args)
          varArgsStr = T.intercalate ", " (map (\(a, t) -> toText a <> ": " <> toTextType t) varArgs)
          allArgs = if null varArgs then argsStr else argsStr <> ", " <> varArgsStr
       in "magic external "
            <> T.pack (show extFunName)
            <> inAngleBracket domStr
            <> ": "
            <> codStr
            <> inParen allArgs
    LM.Global name t ->
      "magic global " <> T.pack (show name) <> ": " <> toTextType t
    LM.OpaqueValue e ->
      "magic opaque" <> inParen (toText e)
    LM.CallType func arg1 arg2 ->
      "magic call-type" <> inParen (toText func <> ", " <> toText arg1 <> ", " <> toText arg2)

showForeignCodType :: FCT.ForeignCodType WT.WeakType -> T.Text
showForeignCodType cod =
  case cod of
    FCT.Cod t -> toTextType t
    FCT.Void -> "void"
