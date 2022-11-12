module Entity.WeakTerm.ToText (toText) where

import Control.Comonad.Cofree
import qualified Data.Text as T
import Entity.Arity
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import qualified Entity.EnumCase as EC
import qualified Entity.EnumTypeName as ET
import qualified Entity.EnumValueName as EV
import Entity.Hint
import qualified Entity.HoleID as HID
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import Entity.Pattern
import qualified Entity.PrimOp as PO
import qualified Entity.PrimType.ToText as PT
import qualified Entity.WeakPrim as WP
import qualified Entity.WeakPrimValue as WPV
import qualified Entity.WeakTerm as WT

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
        LK.Cons {} -> do
          let argStr = inParen $ showItems $ map showArg xts
          showCons ["λ", argStr, toText e]
        -- "<cons>"
        _ -> do
          let argStr = inParen $ showItems $ map showArg xts
          showCons ["λ", argStr, toText e]
    _ :< WT.PiElim e es ->
      showCons $ map toText $ e : es
    _ :< WT.Sigma xts ->
      showCons ["sigma", showItems $ map showArg xts]
    _ :< WT.SigmaIntro es ->
      showCons $ "sigma-intro" : map toText es
    _ :< WT.SigmaElim {} ->
      "<sigma-elim>"
    _ :< WT.Let {} -> do
      "<let>"
    _ :< WT.Prim prim ->
      showPrim prim
    _ :< WT.Aster i es ->
      showCons $ "?M" <> T.pack (show (HID.reify i)) : map toText es
    _ :< WT.Enum l ->
      DD.reify $ ET.reify l
    _ :< WT.EnumIntro (EC.EnumLabel _ _ v) ->
      DD.reify $ EV.reify v
    _ :< WT.EnumElim (e, _) mles -> do
      showCons ["switch", toText e, showItems (map showClause mles)]
    _ :< WT.Question e _ ->
      toText e
    _ :< WT.Magic m -> do
      let a = fmap toText m
      T.pack $ show a
    -- "<magic>"
    -- let es' = map toText es
    -- showCons $ "magic" : T.pack (show i) : es'
    _ :< WT.Match (e, _) caseClause -> do
      showCons $ "case" : toText e : map showCaseClause caseClause

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

showCaseClause :: (PatternF WT.WeakTerm, WT.WeakTerm) -> T.Text
showCaseClause (pat, e) =
  inParen $ showPattern pat <> " " <> toText e

showPattern :: (Hint, DD.DefiniteDescription, Arity, [BinderF WT.WeakTerm]) -> T.Text
showPattern (_, f, _, xts) = do
  case xts of
    [] ->
      inParen $ DD.reify f
    _ -> do
      let xs = map (\(_, x, _) -> x) xts
      inParen $ DD.reify f <> " " <> T.intercalate " " (map showVariable xs)

showClause :: (EC.EnumCase, WT.WeakTerm) -> T.Text
showClause (c, e) =
  inParen $ showCase c <> " " <> toText e

showCase :: EC.EnumCase -> T.Text
showCase c =
  case c of
    _ :< EC.Label (EC.EnumLabel _ _ l) ->
      DD.reify $ EV.reify l
    _ :< EC.Default ->
      "default"
    _ :< EC.Int i ->
      T.pack (show i)

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
