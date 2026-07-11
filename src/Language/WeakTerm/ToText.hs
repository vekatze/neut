module Language.WeakTerm.ToText
  ( Kit (..),
    toTextIndented,
    toTextType,
    toTextTypeVerbose,
    toTextTypeWith,
  )
where

import Control.Comonad.Cofree
import Data.ByteString qualified as BS
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
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.HoleID qualified as HID
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.LamKind qualified as LK
import Language.Common.Literal qualified as L
import Language.Common.LocalDefKind qualified as LDK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.PiKind qualified as PK
import Language.Common.PrimOp qualified as PO
import Language.Common.PrimType.ToText qualified as PT
import Language.Common.Rune qualified as RU
import Language.Common.VarKind qualified as VK
import Language.WeakTerm.WeakPrimValue qualified as WPV
import Language.WeakTerm.WeakTerm qualified as WT

newtype Handle
  = Handle {showDD :: DD.DefiniteDescription -> T.Text}

data Kit = Kit
  { level :: Int,
    distinctVars :: Bool
  }

toTextIndented :: Kit -> WT.WeakTerm -> T.Text
toTextIndented kit term =
  case term of
    _ :< WT.Var x ->
      indentText (level kit) (showVariable (distinctVars kit) x)
    _ :< WT.VarGlobal _ x ->
      indentText (level kit) (DD.localLocator x)
    _ :< WT.PiIntro attr impArgs expArgs defaultArgs e ->
      renderPiIntro kit attr impArgs expArgs defaultArgs e
    _ :< WT.PiElim _ e impArgs expArgs _ ->
      if isIndentedApplication e
        then renderIndentedApplication kit e impArgs expArgs
        else
          indentText
            (level kit)
            (toTextIndented (atLevel kit 0) e <> renderImpArgs impArgs <> inParen (T.intercalate ", " (map (toTextIndented (atLevel kit 0)) expArgs)))
    _ :< WT.PiElimExact e ->
      indentText (level kit) ("exact " <> toTextIndented (atLevel kit 0) e)
    _ :< WT.DataIntro (AttrDI.Attr {..}) consName _ consArgs ->
      indentText (level kit) $
        if isConstLike
          then DD.localLocator consName
          else showApp (DD.localLocator consName) (map (toTextIndented (atLevel kit 0)) consArgs)
    _ :< WT.DataElim isNoetic xets tree ->
      renderDataElim kit isNoetic xets tree
    _ :< WT.BoxIntro letSeq t ->
      indentText (level kit) $
        "box "
          <> T.intercalate ", " (map (renderBoxLet . (\((_, _, x, _), e) -> (x, e))) letSeq)
          <> inBrace (toTextIndented (atLevel kit 0) t)
    _ :< WT.BoxIntroLift _ e ->
      indentText (level kit) ("lift " <> inBrace (toTextIndented (atLevel kit 0) e))
    _ :< WT.BoxElim castSeq (_, k, x, t) e1 _ e2 ->
      indentText (level kit) $
        "letbox "
          <> showVarWithKind (distinctVars kit) k x
          <> ": "
          <> toTextType t
          <> renderCastSuffix castSeq
          <> " = "
          <> toTextIndented (atLevel kit 0) e1
          <> "; "
          <> toTextIndented (atLevel kit 0) e2
    _ :< WT.CodeIntro e ->
      indentText (level kit) ("quote " <> inBrace (toTextIndented (atLevel kit 0) e))
    _ :< WT.CodeElim e ->
      indentText (level kit) ("unquote " <> inBrace (toTextIndented (atLevel kit 0) e))
    _ :< WT.TauIntro ty ->
      indentText (level kit) ("pack-type" <> inParen (toTextType ty))
    _ :< WT.TauElim (_, x) e1 e2 ->
      indentText (level kit) ("unpack-type " <> showVariable (distinctVars kit) x <> " = " <> toTextIndented (atLevel kit 0) e1 <> "; " <> toTextIndented (atLevel kit 0) e2)
    _ :< WT.Actual _ e ->
      indentText (level kit) ("ACTUAL(" <> toTextIndented (atLevel kit 0) e <> ")")
    _ :< WT.Let opacity (_, k, x, t) e1 e2 ->
      renderTermLet kit opacity k x t e1 e2
    _ :< WT.Invoke tropeNames body ->
      indentText (level kit) ("invoke " <> T.intercalate ", " (map DD.localLocator tropeNames) <> "; " <> toTextIndented (atLevel kit 0) body)
    _ :< WT.Prim primValue ->
      indentText (level kit) (showPrimValue primValue)
    _ :< WT.Magic magic ->
      indentText (level kit) (showMagic magic)
    _ :< WT.Annotation _ _ e ->
      toTextIndented kit e

renderBoxLet :: (Ident, WT.WeakTerm) -> T.Text
renderBoxLet (x, e) =
  inParen (Ident.toText x <> ", " <> toTextIndented (Kit 0 False) e)

renderCastSuffix :: [(BinderF WT.WeakType, WT.WeakTerm)] -> T.Text
renderCastSuffix castSeq =
  case map (\((_, _, x, _), _) -> x) castSeq of
    [] ->
      ""
    names ->
      " on " <> T.intercalate ", " (map Ident.toText names)

isIndentedApplication :: WT.WeakTerm -> Bool
isIndentedApplication term =
  case term of
    _ :< WT.PiIntro {} ->
      True
    _ :< WT.Let {} ->
      True
    _ :< WT.DataElim {} ->
      True
    _ ->
      False

renderIndentedApplication :: Kit -> WT.WeakTerm -> ImpArgs.ImpArgs WT.WeakType -> [WT.WeakTerm] -> T.Text
renderIndentedApplication kit function impArgs expArgs =
  toTextIndented kit function
    <> renderImpArgs impArgs
    <> inParen (T.intercalate ", " (map (toTextIndented (atLevel kit 0)) expArgs))

renderImpArgs :: ImpArgs.ImpArgs WT.WeakType -> T.Text
renderImpArgs impArgs =
  case impArgs of
    ImpArgs.Unspecified ->
      ""
    ImpArgs.FullySpecified [] ->
      ""
    ImpArgs.FullySpecified types ->
      inAngleBracket (T.intercalate ", " (map toTextType types))

renderPiIntro :: Kit -> AttrL.Attr WT.WeakType -> [BinderF WT.WeakType] -> [BinderF WT.WeakType] -> [(BinderF WT.WeakType, WT.WeakTerm)] -> WT.WeakTerm -> T.Text
renderPiIntro kit attr impArgs expArgs defaultArgs e =
  indentText (level kit) (renderPiIntroHeader (distinctVars kit) attr impArgs expArgs defaultArgs)
    <> " {\n"
    <> toTextIndented (atLevel kit (level kit + 1)) e
    <> "\n"
    <> indentText (level kit) "}"

renderPiIntroHeader :: Bool -> AttrL.Attr WT.WeakType -> [BinderF WT.WeakType] -> [BinderF WT.WeakType] -> [(BinderF WT.WeakType, WT.WeakTerm)] -> T.Text
renderPiIntroHeader distinct attr impArgs expArgs defaultArgs =
  case AttrL.lamKind attr of
    LK.Fix kind isDestPassing (_, k, x, codType) ->
      LDK.keyword kind
        <> " "
        <> showVarWithKind distinct k x
        <> showSourceImpArgs distinct impArgs
        <> inParen (showFnDomArgList distinct expArgs)
        <> showDefaultArgs defaultArgs
        <> (if isDestPassing then " ->> " else " -> ")
        <> toTextType codType
    LK.Normal _ isDestPassing _ ->
      showSourceImpArgs distinct impArgs
        <> inParen (showFnDomArgList distinct expArgs)
        <> showDefaultArgs defaultArgs
        <> (if isDestPassing then " =>>" else " =>")

showSourceImpArgs :: Bool -> [BinderF WT.WeakType] -> T.Text
showSourceImpArgs distinct impArgs =
  if null impArgs
    then ""
    else inAngleBracket (T.intercalate ", " (map (showSourceImpArg distinct) impArgs))

showSourceImpArg :: Bool -> BinderF WT.WeakType -> T.Text
showSourceImpArg distinct (_, k, x, t) =
  showVarWithKind distinct k x <> ": " <> toTextType t

renderDataElim :: Kit -> Bool -> [(Ident, WT.WeakTerm, WT.WeakType)] -> DT.DecisionTree WT.WeakType WT.WeakTerm -> T.Text
renderDataElim kit isNoetic xets tree =
  indentText (level kit) ((if isNoetic then "case " else "match ") <> showMatchArgs (distinctVars kit) xets <> " {")
    <> "\n"
    <> renderDecisionTree (atLevel kit (level kit + 1)) tree
    <> "\n"
    <> indentText (level kit) "}"

renderDecisionTree :: Kit -> DT.DecisionTree WT.WeakType WT.WeakTerm -> T.Text
renderDecisionTree kit tree =
  case tree of
    DT.Leaf xs letSeq cont ->
      indentText (level kit) ("leaf " <> inBracket (T.intercalate ", " (map (showVariable (distinctVars kit)) xs)) <> " {")
        <> renderLeafLetSeq (atLevel kit (level kit + 1)) letSeq
        <> "\n"
        <> toTextIndented (atLevel kit (level kit + 1)) cont
        <> "\n"
        <> indentText (level kit) "}"
    DT.Unreachable ->
      indentText (level kit) "⊥"
    DT.Switch (cursor, cursorType) (fallbackClause, clauseList) ->
      indentText (level kit) ("switch (" <> showVariable (distinctVars kit) cursor <> ": " <> toTextType cursorType <> ") {")
        <> T.concat (map (renderDecisionCase kit) clauseList)
        <> case clauseList of
          [] -> "\n"
          _ -> "\n"
        <> indentText (level kit) "| default =>"
        <> "\n"
        <> renderDecisionTree (atLevel kit (level kit + 1)) fallbackClause
        <> "\n"
        <> indentText (level kit) "}"

renderLeafLetSeq :: Kit -> [(BinderF WT.WeakType, WT.WeakTerm)] -> T.Text
renderLeafLetSeq kit letSeq =
  T.concat (map (renderLeafLet kit) letSeq)

renderLeafLet :: Kit -> (BinderF WT.WeakType, WT.WeakTerm) -> T.Text
renderLeafLet kit ((_, k, x, t), e) =
  "\n"
    <> indentText (level kit) (showVarWithKind (distinctVars kit) k x <> ": " <> toTextType t <> " = " <> toTextIndented (atLevel kit 0) e)

renderDecisionCase :: Kit -> DT.Case WT.WeakType WT.WeakTerm -> T.Text
renderDecisionCase kit decisionCase =
  "\n"
    <> indentText (level kit) ("| " <> renderDecisionCaseLabel (distinctVars kit) decisionCase <> " =>")
    <> "\n"
    <> renderDecisionCaseBody (atLevel kit (level kit + 1)) decisionCase

renderDecisionCaseBody :: Kit -> DT.Case WT.WeakType WT.WeakTerm -> T.Text
renderDecisionCaseBody kit decisionCase =
  case decisionCase of
    DT.LiteralCase _ _ cont ->
      renderDecisionTree kit cont
    DT.ConsCase (DT.ConsCaseRecord {..}) ->
      renderDecisionTree kit cont

renderDecisionCaseLabel :: Bool -> DT.Case WT.WeakType WT.WeakTerm -> T.Text
renderDecisionCaseLabel distinct decisionCase =
  case decisionCase of
    DT.LiteralCase _ i _ ->
      renderLiteral i
    DT.ConsCase (DT.ConsCaseRecord {..}) ->
      DD.localLocator consDD
        <> if isConstLike
          then ""
          else "(" <> T.intercalate ", " (map (renderConstructorArg distinct) consArgs) <> ")"

renderLiteral :: L.Literal -> T.Text
renderLiteral literal =
  case literal of
    L.Int value ->
      T.pack (show value)
    L.Rune rune ->
      T.pack (show rune)

renderConstructorArg :: Bool -> BinderF WT.WeakType -> T.Text
renderConstructorArg distinct (_, _, x, _) =
  showVariable distinct x

renderTermLet :: Kit -> WT.LetOpacity -> VK.VarKind -> Ident -> WT.WeakType -> WT.WeakTerm -> WT.WeakTerm -> T.Text
renderTermLet kit opacity k x t e1 e2 =
  renderTermBinding
    kit
    ((case opacity of WT.Noetic -> "tie "; _ -> "let ") <> showVarWithKind (distinctVars kit) k x <> ": " <> toTextType t <> " =")
    e1
    e2

renderTermBinding :: Kit -> T.Text -> WT.WeakTerm -> WT.WeakTerm -> T.Text
renderTermBinding kit header value cont =
  case value of
    _ :< WT.PiIntro {} ->
      renderMultilineTermBinding kit header value cont
    _ :< WT.DataElim {} ->
      renderMultilineTermBinding kit header value cont
    _ :< WT.Let {} ->
      renderBlockTermBinding kit header value cont
    _ ->
      indentText (level kit) (header <> " " <> toTextIndented (atLevel kit 0) value)
        <> ";\n"
        <> toTextIndented kit cont

renderMultilineTermBinding :: Kit -> T.Text -> WT.WeakTerm -> WT.WeakTerm -> T.Text
renderMultilineTermBinding kit header value cont =
  indentText (level kit) header
    <> "\n"
    <> toTextIndented (atLevel kit (level kit + 1)) value
    <> ";\n"
    <> toTextIndented kit cont

renderBlockTermBinding :: Kit -> T.Text -> WT.WeakTerm -> WT.WeakTerm -> T.Text
renderBlockTermBinding kit header value cont =
  indentText (level kit) (header <> " {")
    <> "\n"
    <> toTextIndented (atLevel kit (level kit + 1)) value
    <> "\n"
    <> indentText (level kit) "};"
    <> "\n"
    <> toTextIndented kit cont

atLevel :: Kit -> Int -> Kit
atLevel kit level' =
  kit {level = level'}

indentText :: Int -> T.Text -> T.Text
indentText level text =
  T.intercalate "\n" (map (T.replicate (level * 2) " " <>) (T.lines text))

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
      showVariable False x
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

showFnDomArgList :: Bool -> [BinderF WT.WeakType] -> T.Text
showFnDomArgList distinct args =
  T.intercalate ", " (map (showFnDomArg distinct) args)

showMatchArgs :: Bool -> [(Ident, WT.WeakTerm, WT.WeakType)] -> T.Text
showMatchArgs distinct =
  showMatchArgs' distinct

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
  showVarWithKind False k x

showDefaultDomArg' :: Handle -> (BinderF WT.WeakType, WT.WeakTerm) -> T.Text
showDefaultDomArg' h ((_, k, x, t), defaultValue) =
  showVarWithKind False k x <> ": " <> toTextType' h t <> " := " <> toTextIndented (Kit 0 False) defaultValue

showDefaultDomBinder' :: Handle -> BinderF WT.WeakType -> T.Text
showDefaultDomBinder' h (_, k, x, t) =
  showVarWithKind False k x <> ": " <> toTextType' h t

showDataImpArgWithDefault' :: Handle -> (BinderF WT.WeakType, Maybe WT.WeakTerm) -> T.Text
showDataImpArgWithDefault' h ((_, k, x, t), maybeDefault) = do
  let baseArg =
        case t of
          _ :< WT.Tau -> showVarWithKind False k x
          _ -> "(" <> showVarWithKind False k x <> ": " <> toTextType' h t <> ")"
  case maybeDefault of
    Nothing -> baseArg
    Just defaultValue -> "(" <> baseArg <> " := " <> toTextIndented (Kit 0 False) defaultValue <> ")"

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

showDomArg' :: Handle -> BinderF WT.WeakType -> T.Text
showDomArg' h (_, _, _, t) =
  toTextType' h t

showFnDomArg :: Bool -> BinderF WT.WeakType -> T.Text
showFnDomArg distinct (_, k, x, t) =
  showVarWithKind distinct k x <> ": " <> toTextType t

showVarWithKind :: Bool -> VK.VarKind -> Ident -> T.Text
showVarWithKind distinct k x =
  case k of
    VK.Exp -> "!" <> showVariable distinct x
    VK.Normal -> showVariable distinct x

showDomArgList' :: Handle -> [BinderF WT.WeakType] -> T.Text
showDomArgList' h mxts =
  T.intercalate ", " $ map (showDomArg' h) mxts

showApp :: T.Text -> [T.Text] -> T.Text
showApp e es =
  e <> inParen (T.intercalate ", " es)

showVariable :: Bool -> Ident -> T.Text
showVariable distinct x =
  if isHole x
    then "_"
    else if distinct then Ident.toText' x else Ident.toText x

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
    WPV.String _ _ bytes ->
      T.pack $ show $ BS.unpack bytes
    WPV.NoeticString _ text ->
      T.pack $ show text
    WPV.NoeticBinary _ bytes ->
      T.pack $ show $ BS.unpack bytes
    WPV.Text text ->
      T.pack $ show text
    WPV.Blob bytes ->
      T.pack $ show $ BS.unpack bytes
    WPV.Rune r ->
      "`" <> T.replace "`" "\\`" (RU.asText r) <> "`"

showMatchArgs' :: Bool -> [(Ident, WT.WeakTerm, WT.WeakType)] -> T.Text
showMatchArgs' distinct xets = do
  inParen $ T.intercalate ", " (map (showMatchArg' distinct) xets)

showMatchArg' :: Bool -> (Ident, WT.WeakTerm, WT.WeakType) -> T.Text
showMatchArg' distinct (x, e, t) = do
  showVariable distinct x <> ": " <> toTextType t <> " = " <> toTextIndented (Kit 0 distinct) e

showMagic' :: Handle -> M.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm -> T.Text
showMagic' h (M.WeakMagic magic) =
  case magic of
    M.LowMagic lowMagic ->
      showLowMagic' h lowMagic
    M.Calloc sizeType num size ->
      "magic calloc" <> inParen (toTextType' h sizeType <> ", " <> toTextIndented (Kit 0 False) num <> ", " <> toTextIndented (Kit 0 False) size)
    M.Malloc sizeType size ->
      "magic malloc" <> inParen (toTextType' h sizeType <> ", " <> toTextIndented (Kit 0 False) size)
    M.Realloc sizeType ptr size ->
      "magic realloc" <> inParen (toTextType' h sizeType <> ", " <> toTextIndented (Kit 0 False) ptr <> ", " <> toTextIndented (Kit 0 False) size)
    M.Free _ ptr ->
      "magic free" <> inParen (toTextIndented (Kit 0 False) ptr)
    M.InspectType _ typeValueExpr e ->
      "magic inspect-type" <> inParen (toTextType' h typeValueExpr <> ", " <> toTextType' h e)
    M.EqType _ typeExpr1 typeExpr2 ->
      "magic eq-type" <> inParen (toTextType' h typeExpr1 <> ", " <> toTextType' h typeExpr2)
    M.ShowType typeExpr ->
      "magic show-type" <> inParen (toTextType' h typeExpr)
    M.AssertMixable _ unitTypeExpr typeExpr ->
      "magic assert-mixable" <> inParen (toTextType' h unitTypeExpr <> ", " <> toTextType' h typeExpr)
    M.TextCons rune text ->
      "magic text-cons" <> inParen (toTextIndented (Kit 0 False) rune <> ", " <> toTextIndented (Kit 0 False) text)
    M.TextUncons _ text ->
      "magic text-uncons" <> inParen (toTextIndented (Kit 0 False) text)
    M.MakeSwitch _ key fallback clauses ->
      "magic make-switch" <> inParen (toTextIndented (Kit 0 False) key <> ", " <> toTextIndented (Kit 0 False) fallback <> ", " <> toTextIndented (Kit 0 False) clauses)
    M.CompileError msg ->
      "magic compile-error" <> inParen (toTextIndented (Kit 0 False) msg)
    M.GetOriginFileName ->
      "magic get-origin-file-name()"
    M.GetOriginLine ->
      "magic get-origin-line()"
    M.GetOriginColumn ->
      "magic get-origin-column()"

showLowMagic' :: Handle -> LM.LowMagic WT.WeakType WT.WeakType WT.WeakTerm -> T.Text
showLowMagic' h lowMagic =
  case lowMagic of
    LM.Cast from to value ->
      "magic cast" <> inParen (toTextType' h from <> ", " <> toTextType' h to <> ", " <> toTextIndented (Kit 0 False) value)
    LM.Store t unit value pointer ->
      "magic store"
        <> inParen
          ( toTextType' h t
              <> ", "
              <> toTextType' h unit
              <> ", "
              <> toTextIndented (Kit 0 False) value
              <> ", "
              <> toTextIndented (Kit 0 False) pointer
          )
    LM.Load t pointer ->
      "magic load" <> inParen (toTextType' h t <> ", " <> toTextIndented (Kit 0 False) pointer)
    LM.Alloca t size ->
      "magic alloca" <> inParen (toTextType' h t <> ", " <> toTextIndented (Kit 0 False) size)
    LM.External domList cod extFunName args varArgs -> do
      let domStr = T.intercalate ", " (map (toTextType' h) domList)
      let codStr = showForeignCodType' h cod
      let argsStr = T.intercalate ", " (map (toTextIndented (Kit 0 False)) args)
      let varArgsStr = T.intercalate ", " (map (\(a, t) -> toTextIndented (Kit 0 False) a <> ": " <> toTextType' h t) varArgs)
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
      "magic opaque" <> inParen (toTextIndented (Kit 0 False) e)
    LM.CallType func arg1 arg2 arg3 ->
      "magic call-type" <> inParen (toTextIndented (Kit 0 False) func <> ", " <> toTextIndented (Kit 0 False) arg1 <> ", " <> toTextIndented (Kit 0 False) arg2 <> ", " <> toTextIndented (Kit 0 False) arg3)

showForeignCodType' :: Handle -> FCT.ForeignCodType WT.WeakType -> T.Text
showForeignCodType' h cod =
  case cod of
    FCT.Cod t -> toTextType' h t
    FCT.Void -> "void"
