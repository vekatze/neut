module Language.WeakTerm.ToText
  ( toText,
    toTextType,
    toTextTypeVerbose,
    toTextTypeWith,
  )
where

import Control.Comonad.Cofree
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Kernel.Common.Module (Module)
import Kernel.Common.ReadableDD qualified as ReadableDD
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

newtype Handle =
  Handle {showDD :: DD.DefiniteDescription -> T.Text}

toText :: WT.WeakTerm -> T.Text
toText term =
  case term of
    _ :< WT.Var x ->
      showVariable x
    _ :< WT.VarGlobal _ x ->
      DD.localLocator x
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
            <> showImpArgs impArgs
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
            <> showImpArgs impArgs
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
        then DD.localLocator consName
        else showApp (DD.localLocator consName) (map toText consArgs)
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
toTextType =
  toTextType' (Handle DD.localLocator)

toTextTypeVerbose :: Module -> WT.WeakType -> T.Text
toTextTypeVerbose baseModule =
  toTextType' (Handle (ReadableDD.readableDD' baseModule))

toTextTypeWith :: (DD.DefiniteDescription -> T.Text) -> WT.WeakType -> T.Text
toTextTypeWith f =
  toTextType' (Handle f)

toTextType' :: Handle -> WT.WeakType -> T.Text
toTextType' h ty =
  case ty of
    _ :< WT.Tau ->
      "type"
    _ :< WT.TVar x ->
      showVariable x
    _ :< WT.TVarGlobal _ x ->
      showDD h x
    _ :< WT.TyApp t args -> do
      case t of
        _ :< WT.TVarGlobal attr _
          | AttrVG.isConstLike attr ->
              toTextType' h t
        _ ->
          showApp (toTextType' h t) (map (toTextType' h) args)
    _ :< WT.Pi piKind impArgs expArgs defaultArgs cod -> do
      case piKind of
        PK.Normal isConstLike ->
          if isConstLike
            then showImpArgsForAll' h impArgs defaultArgs <> toTextType' h cod
            else showImpArgs impArgs <> inParen (showDomArgList' h expArgs) <> showDefaultBinders' h defaultArgs <> " -> " <> toTextType' h cod
        PK.DestPass isConstLike ->
          if isConstLike
            then showImpArgsForAll' h impArgs defaultArgs <> toTextType' h cod
            else showImpArgs impArgs <> inParen (showDomArgList' h expArgs) <> showDefaultBinders' h defaultArgs <> " ->> " <> toTextType' h cod
        PK.DataIntro _ -> do
          showImpArgsForAll' h impArgs defaultArgs <> toTextType' h cod
    _ :< WT.Data (AttrD.Attr {..}) name es -> do
      if isConstLike
        then showDD h name
        else showApp (showDD h name) (map (toTextType' h) es)
    _ :< WT.Box t ->
      "+" <> toTextType' h t
    _ :< WT.BoxNoema t ->
      "&" <> toTextType' h t
    _ :< WT.Code t ->
      "'" <> toTextType' h t
    _ :< WT.PrimType pt ->
      PT.toText pt
    _ :< WT.Void ->
      "void"
    _ :< WT.Resource dd _ -> do
      showDD h dd
    _ :< WT.TypeHole i es ->
      "?" <> T.pack (show (HID.reify i)) <> "(" <> T.intercalate "," (map (toTextType' h) es) <> ")"

showImpArgs :: [BinderF WT.WeakType] -> T.Text
showImpArgs impArgs =
  if null impArgs
    then ""
    else inAngleBracket $ showImpDomArgList impArgs

showDefaultArgs :: [(BinderF WT.WeakType, WT.WeakTerm)] -> T.Text
showDefaultArgs =
  showDefaultArgs' (Handle DD.localLocator)

showFnDomArgList :: [BinderF WT.WeakType] -> T.Text
showFnDomArgList =
  showFnDomArgList' (Handle DD.localLocator)

showMatchArgs :: [(Ident, WT.WeakTerm, WT.WeakType)] -> T.Text
showMatchArgs =
  showMatchArgs' (Handle DD.localLocator)

showDecisionTree :: DT.DecisionTree WT.WeakType WT.WeakTerm -> T.Text
showDecisionTree =
  showDecisionTree' (Handle DD.localLocator)

showMagic :: M.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm -> T.Text
showMagic =
  showMagic' (Handle DD.localLocator)

showDefaultArgs' :: Handle -> [(BinderF WT.WeakType, WT.WeakTerm)] -> T.Text
showDefaultArgs' h defaultArgs =
  if null defaultArgs
    then ""
    else inBracket $ showDefaultDomArgList' h defaultArgs

showImpArgsForAll' :: Handle -> [BinderF WT.WeakType] -> [BinderF WT.WeakType] -> T.Text
showImpArgsForAll' h impArgs defaultArgs = do
  let impArgsWithDefaults = map (,Nothing) impArgs ++ map (,Nothing) defaultArgs
  if null impArgsWithDefaults
    then ""
    else "∀ " <> T.intercalate " " (map (showDataImpArgWithDefault' h) impArgsWithDefaults) <> ". "

showDefaultBinders' :: Handle -> [BinderF WT.WeakType] -> T.Text
showDefaultBinders' h defaultArgs =
  if null defaultArgs
    then ""
    else inBracket $ showDefaultDomBinderList' h defaultArgs

showImpDomArgList :: [BinderF WT.WeakType] -> T.Text
showImpDomArgList mxts =
  T.intercalate ", " $ map showImpDomArg mxts

showDefaultDomArgList' :: Handle -> [(BinderF WT.WeakType, WT.WeakTerm)] -> T.Text
showDefaultDomArgList' h mxts =
  T.intercalate ", " $ map (showDefaultDomArg' h) mxts

showDefaultDomBinderList' :: Handle -> [BinderF WT.WeakType] -> T.Text
showDefaultDomBinderList' h mxts =
  T.intercalate ", " $ map (showDefaultDomBinder' h) mxts

showImpDomArg :: BinderF WT.WeakType -> T.Text
showImpDomArg (_, k, x, _) =
  showVarWithKind k x

showDefaultDomArg' :: Handle -> (BinderF WT.WeakType, WT.WeakTerm) -> T.Text
showDefaultDomArg' h ((_, k, x, t), defaultValue) =
  showVarWithKind k x <> ": " <> toTextType' h t <> " := " <> toText defaultValue

showDefaultDomBinder' :: Handle -> BinderF WT.WeakType -> T.Text
showDefaultDomBinder' h (_, k, x, t) =
  showVarWithKind k x <> ": " <> toTextType' h t

showDataImpArgWithDefault' :: Handle -> (BinderF WT.WeakType, Maybe WT.WeakTerm) -> T.Text
showDataImpArgWithDefault' h ((_, k, x, t), maybeDefault) = do
  let baseArg =
        case t of
          _ :< WT.Tau -> showVarWithKind k x
          _ -> "(" <> showVarWithKind k x <> ": " <> toTextType' h t <> ")"
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

showTypeArgs' :: Handle -> [BinderF WT.WeakType] -> T.Text
showTypeArgs' h args =
  case args of
    [] ->
      T.empty
    [(_, k, x, t)] ->
      inParen $ showVarWithKind k x <> " " <> toTextType' h t
    (_, k, x, t) : xts -> do
      let s1 = inParen $ showVarWithKind k x <> " " <> toTextType' h t
      let s2 = showTypeArgs' h xts
      s1 <> " " <> s2

showDomArg' :: Handle -> BinderF WT.WeakType -> T.Text
showDomArg' h (_, _, _, t) =
  toTextType' h t

showFnDomArg' :: Handle -> BinderF WT.WeakType -> T.Text
showFnDomArg' h (_, k, x, t) =
  showVarWithKind k x <> ": " <> toTextType' h t

showVarWithKind :: VK.VarKind -> Ident -> T.Text
showVarWithKind k x =
  case k of
    VK.Exp -> "!" <> showVariable x
    VK.Normal -> showVariable x

showDomArgList' :: Handle -> [BinderF WT.WeakType] -> T.Text
showDomArgList' h mxts =
  T.intercalate ", " $ map (showDomArg' h) mxts

showFnDomArgList' :: Handle -> [BinderF WT.WeakType] -> T.Text
showFnDomArgList' h mxts =
  T.intercalate ", " $ map (showFnDomArg' h) mxts

showApp :: T.Text -> [T.Text] -> T.Text
showApp e es =
  e <> inParen (T.intercalate ", " es)

showVariable :: Ident -> T.Text
showVariable x =
  if isHole x
    then "_"
    else Ident.toText x

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
    WPV.NoeticString _ text ->
      T.pack $ show text
    WPV.Text text ->
      T.pack $ show text
    WPV.Rune r ->
      "`" <> T.replace "`" "\\`" (RU.asText r) <> "`"

showMatchArgs' :: Handle -> [(Ident, WT.WeakTerm, WT.WeakType)] -> T.Text
showMatchArgs' h xets = do
  inParen $ T.intercalate ", " (map (showMatchArg' h) xets)

showMatchArg' :: Handle -> (Ident, WT.WeakTerm, WT.WeakType) -> T.Text
showMatchArg' h (x, e, t) = do
  showVariable x <> ": " <> toTextType' h t <> " = " <> toText e

showLeafLetItem' :: Handle -> (BinderF WT.WeakType, WT.WeakTerm) -> T.Text
showLeafLetItem' h ((_, k, x, t), e) =
  showVarWithKind k x <> ": " <> toTextType' h t <> " = " <> toText e

showDecisionTree' :: Handle -> DT.DecisionTree WT.WeakType WT.WeakTerm -> T.Text
showDecisionTree' h tree =
  case tree of
    DT.Leaf xs letSeq cont -> do
      showApp
        "leaf"
        [ inBracket (T.intercalate ", " (map showVariable xs)),
          inParen $ T.intercalate ", " (map (showLeafLetItem' h) letSeq),
          toText cont
        ]
    DT.Unreachable ->
      "UNREACHABLE"
    DT.Switch (cursor, cursorType) (fallbackClause, clauseList) -> do
      "switch"
        <> inParen
          ( showVariable cursor
              <> ": "
              <> toTextType' h cursorType
          )
        <> inBrace (T.intercalate ", " (map (showClauseList' h) clauseList ++ [showDecisionTree' h fallbackClause]))

showClauseList' :: Handle -> DT.Case WT.WeakType WT.WeakTerm -> T.Text
showClauseList' h decisionCase = do
  case decisionCase of
    DT.LiteralCase _ i cont -> do
      showApp "literal" [T.pack (show i), showDecisionTree' h cont]
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      showApp
        (showDD h consDD)
        [ T.pack (show (D.reify disc)),
          inParen $ T.intercalate ", " $ map (\(e, t) -> toTextType' h e <> ": " <> toTextType' h t) dataArgs,
          inParen $ showTypeArgs' h consArgs,
          showDecisionTree' h cont
        ]

showMagic' :: Handle -> M.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm -> T.Text
showMagic' h (M.WeakMagic magic) =
  case magic of
    M.LowMagic lowMagic ->
      showLowMagic' h lowMagic
    M.Calloc sizeType num size ->
      "magic calloc" <> inParen (toTextType' h sizeType <> ", " <> toText num <> ", " <> toText size)
    M.Malloc sizeType size ->
      "magic malloc" <> inParen (toTextType' h sizeType <> ", " <> toText size)
    M.Realloc sizeType ptr size ->
      "magic realloc" <> inParen (toTextType' h sizeType <> ", " <> toText ptr <> ", " <> toText size)
    M.Free _ ptr ->
      "magic free" <> inParen (toText ptr)
    M.InspectType _ typeValueExpr e ->
      "magic inspect-type" <> inParen (toTextType' h typeValueExpr <> ", " <> toTextType' h e)
    M.EqType _ typeExpr1 typeExpr2 ->
      "magic eq-type" <> inParen (toTextType' h typeExpr1 <> ", " <> toTextType' h typeExpr2)
    M.ShowType stringTypeExpr typeExpr ->
      "magic show-type" <> inParen (toTextType' h stringTypeExpr <> ", " <> toTextType' h typeExpr)
    M.StringCons stringTypeExpr rune text ->
      "magic text-cons" <> inParen (toTextType' h stringTypeExpr <> ", " <> toText rune <> ", " <> toText text)
    M.StringUncons _ text ->
      "magic text-uncons" <> inParen (toText text)
    M.CompileError typeExpr msg ->
      "magic compile-error" <> inParen (toTextType' h typeExpr <> ", " <> toText msg)

showLowMagic' :: Handle -> LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm -> T.Text
showLowMagic' h lowMagic =
  case lowMagic of
    LM.Cast from to value ->
      "magic cast" <> inParen (toTextType' h from <> ", " <> toTextType' h to <> ", " <> toText value)
    LM.Store t unit value pointer ->
      "magic store"
        <> inParen
          ( toTextType' h t
              <> ", "
              <> toTextType' h unit
              <> ", "
              <> toText value
              <> ", "
              <> toText pointer
          )
    LM.Load t pointer ->
      "magic load" <> inParen (toTextType' h t <> ", " <> toText pointer)
    LM.Alloca t size ->
      "magic alloca" <> inParen (toTextType' h t <> ", " <> toText size)
    LM.External domList cod extFunName args varArgs -> do
      let domStr = T.intercalate ", " (map (toTextType' h) domList)
      let codStr = showForeignCodType' h cod
      let argsStr = T.intercalate ", " (map toText args)
      let varArgsStr = T.intercalate ", " (map (\(a, t) -> toText a <> ": " <> toTextType' h t) varArgs)
      let allArgs = if null varArgs then argsStr else argsStr <> ", " <> varArgsStr
      "magic external "
        <> T.pack (show extFunName)
        <> inAngleBracket domStr
        <> ": "
        <> codStr
        <> inParen allArgs
    LM.Global name t ->
      "magic global " <> T.pack (show name) <> ": " <> toTextType' h t
    LM.OpaqueValue e ->
      "magic opaque" <> inParen (toText e)
    LM.CallType func arg1 arg2 ->
      "magic call-type" <> inParen (toText func <> ", " <> toText arg1 <> ", " <> toText arg2)

showForeignCodType' :: Handle -> FCT.ForeignCodType WT.WeakType -> T.Text
showForeignCodType' h cod =
  case cod of
    FCT.Cod t -> toTextType' h t
    FCT.Void -> "void"
