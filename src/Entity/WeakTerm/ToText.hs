module Entity.WeakTerm.ToText (toText) where

import Control.Comonad.Cofree
import qualified Data.Text as T
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import Entity.EnumCase
import qualified Entity.EnumTypeName as ET
import qualified Entity.EnumValueName as EV
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.Pattern
import Entity.WeakTerm

toText :: WeakTerm -> T.Text
toText term =
  case term of
    _ :< WeakTermTau ->
      "tau"
    _ :< WeakTermVar x ->
      showVariable x
    _ :< WeakTermVarGlobal x ->
      T.pack $ show x
    _ :< WeakTermPi xts cod
      | [(_, I ("internal.sigma-tau", _), _), (_, _, _ :< WeakTermPi yts _)] <- xts ->
        case splitLast yts of
          Nothing ->
            "(product)"
          Just (zts, (_, _, t)) ->
            showCons ["∑", inParen $ showTypeArgs zts, toText t]
      | otherwise ->
        showCons ["Π", inParen $ showTypeArgs xts, toText cod]
    _ :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix (_, x, _) -> do
          let argStr = inParen $ showItems $ map showArg xts
          showCons ["fix", showVariable x, argStr, toText e]
        LamKindCons {} -> do
          let argStr = inParen $ showItems $ map showArg xts
          showCons ["λ", argStr, toText e]
        -- "<cons>"
        _ -> do
          let argStr = inParen $ showItems $ map showArg xts
          showCons ["λ", argStr, toText e]
    _ :< WeakTermPiElim e es ->
      showCons $ map toText $ e : es
    _ :< WeakTermSigma xts ->
      showCons ["sigma", showItems $ map showArg xts]
    _ :< WeakTermSigmaIntro es ->
      showCons $ "sigma-intro" : map toText es
    _ :< WeakTermSigmaElim {} ->
      "<sigma-elim>"
    _ :< WeakTermLet {} -> do
      "<let>"
    _ :< WeakTermPrim x ->
      T.pack $ show x -- fixme
    _ :< WeakTermAster i ->
      "?M" <> T.pack (show i)
    _ :< WeakTermInt _ a ->
      T.pack $ show a
    _ :< WeakTermFloat _ a ->
      T.pack $ show a
    _ :< WeakTermEnum l ->
      DD.reify $ ET.reify l
    _ :< WeakTermEnumIntro (EnumLabel _ _ v) ->
      DD.reify $ EV.reify v
    _ :< WeakTermEnumElim (e, _) mles -> do
      showCons ["switch", toText e, showItems (map showClause mles)]
    _ :< WeakTermQuestion e _ ->
      toText e
    _ :< WeakTermMagic m -> do
      let a = fmap toText m
      T.pack $ show a
    -- "<magic>"
    -- let es' = map toText es
    -- showCons $ "magic" : T.pack (show i) : es'
    _ :< WeakTermMatch mSubject (e, _) caseClause -> do
      case mSubject of
        Nothing -> do
          showCons $ "case" : toText e : map showCaseClause caseClause
        Just _ -> do
          showCons $ "case-noetic" : toText e : map showCaseClause caseClause
    _ :< WeakTermNoema s e ->
      showCons ["&" <> toText s, toText e]
    _ :< WeakTermNoemaIntro s e ->
      showCons ["noema-intro", Ident.toText s, toText e]
    _ :< WeakTermNoemaElim s e ->
      showCons ["noema-elim", Ident.toText s, toText e]
    _ :< WeakTermArray elemType ->
      showCons ["array", toText elemType]
    _ :< WeakTermArrayIntro elemType elems ->
      showCons $ "array-new" : toText elemType : map toText elems
    _ :< WeakTermArrayAccess subject elemType array index ->
      showCons ["array-access", toText subject, toText elemType, toText array, toText index]
    _ :< WeakTermText ->
      "text"
    _ :< WeakTermTextIntro text ->
      T.pack $ show text
    _ :< WeakTermCell contentType ->
      showCons ["cell", toText contentType]
    _ :< WeakTermCellIntro _ content ->
      showCons ["cell-new", toText content]
    _ :< WeakTermCellRead cell ->
      showCons ["cell-read", toText cell]
    _ :< WeakTermCellWrite cell newValue ->
      showCons ["cell-write", toText cell, toText newValue]
    _ :< WeakTermResourceType name ->
      DD.reify name

inParen :: T.Text -> T.Text
inParen s =
  "(" <> s <> ")"

showArg :: (Hint, Ident, WeakTerm) -> T.Text
showArg (_, x, t) =
  inParen $ showVariable x <> " " <> toText t

showTypeArgs :: [BinderF WeakTerm] -> T.Text
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
  Ident.toText

showCaseClause :: (PatternF WeakTerm, WeakTerm) -> T.Text
showCaseClause (pat, e) =
  inParen $ showPattern pat <> " " <> toText e

showPattern :: (Hint, DD.DefiniteDescription, [BinderF WeakTerm]) -> T.Text
showPattern (_, f, xts) = do
  case xts of
    [] ->
      inParen $ DD.reify f
    _ -> do
      let xs = map (\(_, x, _) -> x) xts
      inParen $ DD.reify f <> " " <> T.intercalate " " (map showVariable xs)

showClause :: (EnumCase, WeakTerm) -> T.Text
showClause (c, e) =
  inParen $ showCase c <> " " <> toText e

showCase :: EnumCase -> T.Text
showCase c =
  case c of
    _ :< EnumCaseLabel (EnumLabel _ _ l) ->
      DD.reify $ EV.reify l
    _ :< EnumCaseDefault ->
      "default"
    _ :< EnumCaseInt i ->
      T.pack (show i)

showItems :: [T.Text] -> T.Text
showItems =
  T.intercalate " "

showCons :: [T.Text] -> T.Text
showCons =
  inParen . T.intercalate " "

splitLast :: [a] -> Maybe ([a], a)
splitLast xs =
  if null xs
    then Nothing
    else Just (init xs, last xs)
