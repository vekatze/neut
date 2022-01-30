{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.WeakTerm where

import Control.Comonad.Cofree (Cofree (..))
import Data.Basic
  ( BinderF,
    EnumCase,
    EnumCaseF (..),
    Hint,
    Ident (..),
    LamKindF (LamKindCons, LamKindFix),
    PatternF,
    asText,
    fromLamKind,
  )
import Data.Binary (Binary)
import qualified Data.IntMap as IntMap
import Data.LowType (Magic, showIntSize)
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.PQueue.Min as Q
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics (Generic)

data WeakTermF a
  = WeakTermTau
  | WeakTermVar Ident
  | WeakTermVarGlobal T.Text
  | WeakTermPi [BinderF a] a
  | WeakTermPiIntro (LamKindF a) [BinderF a] a
  | WeakTermPiElim a [a]
  | WeakTermSigma [BinderF a]
  | WeakTermSigmaIntro [a]
  | WeakTermSigmaElim [BinderF a] a a
  | WeakTermAster Int
  | WeakTermConst T.Text
  | WeakTermInt a Integer
  | WeakTermFloat a Double
  | WeakTermEnum T.Text
  | WeakTermEnumIntro T.Text
  | WeakTermEnumElim (a, a) [(EnumCase, a)]
  | WeakTermQuestion a a -- e : t (output the type `t` as note)
  | WeakTermMagic (Magic a) -- (magic kind arg-1 ... arg-n)
  | WeakTermMatch
      (Maybe a) -- noetic subject (this is for `case-noetic`)
      (a, a) -- (pattern-matched value, its type)
      [(PatternF a, a)]
  | WeakTermNoema a a
  | WeakTermNoemaIntro Ident a
  | WeakTermNoemaElim Ident a
  | WeakTermArray a a
  | WeakTermArrayIntro a [a]
  | WeakTermArrayAccess a a a a
  deriving (Generic)

type WeakTerm = Cofree WeakTermF Hint

instance (Binary a) => Binary (WeakTermF a)

instance Binary WeakTerm

type DefInfo =
  ((Hint, T.Text), [BinderF WeakTerm], WeakTerm, WeakTerm)

type TopDefInfo =
  ((Hint, T.Text), [BinderF WeakTerm], [BinderF WeakTerm], WeakTerm, WeakTerm)

type SubstWeakTerm =
  IntMap.IntMap WeakTerm

type Constraint =
  (WeakTerm, WeakTerm) -- (expected-type, actual-type)

type MetaVarSet =
  S.Set Int

data ConstraintKind
  = ConstraintKindDelta Constraint
  | ConstraintKindOther

newtype SuspendedConstraint
  = SuspendedConstraint (MetaVarSet, ConstraintKind, (Constraint, Constraint))

instance Eq SuspendedConstraint where
  (SuspendedConstraint (_, kind1, _)) == (SuspendedConstraint (_, kind2, _)) =
    kindToInt kind1 == kindToInt kind2

instance Ord SuspendedConstraint where
  (SuspendedConstraint (_, kind1, _)) `compare` (SuspendedConstraint (_, kind2, _)) =
    kindToInt kind1 `compare` kindToInt kind2

type SuspendedConstraintQueue =
  Q.MinQueue SuspendedConstraint

toVar :: Hint -> Ident -> WeakTerm
toVar m x =
  m :< WeakTermVar x

kindToInt :: ConstraintKind -> Int
kindToInt k =
  case k of
    ConstraintKindDelta {} ->
      0
    ConstraintKindOther {} ->
      1

i8 :: Hint -> WeakTerm
i8 m =
  m :< WeakTermConst (showIntSize 8)

i64 :: Hint -> WeakTerm
i64 m =
  m :< WeakTermConst (showIntSize 64)

varWeakTerm :: WeakTerm -> S.Set Ident
varWeakTerm term =
  case term of
    _ :< WeakTermTau ->
      S.empty
    _ :< WeakTermVar x ->
      S.singleton x
    _ :< WeakTermVarGlobal {} ->
      S.empty
    _ :< WeakTermPi xts t ->
      varWeakTerm' xts [t]
    _ :< WeakTermPiIntro k xts e ->
      varWeakTerm' (catMaybes [fromLamKind k] ++ xts) [e]
    _ :< WeakTermPiElim e es -> do
      let xs = varWeakTerm e
      let ys = S.unions $ map varWeakTerm es
      S.union xs ys
    _ :< WeakTermSigma xts ->
      varWeakTerm' xts []
    _ :< WeakTermSigmaIntro es ->
      S.unions $ map varWeakTerm es
    _ :< WeakTermSigmaElim xts e1 e2 -> do
      let set1 = varWeakTerm e1
      let set2 = varWeakTerm' xts [e2]
      S.union set1 set2
    _ :< WeakTermConst _ ->
      S.empty
    _ :< WeakTermAster _ ->
      S.empty
    _ :< WeakTermInt t _ ->
      varWeakTerm t
    _ :< WeakTermFloat t _ ->
      varWeakTerm t
    _ :< WeakTermEnum _ ->
      S.empty
    _ :< WeakTermEnumIntro _ ->
      S.empty
    _ :< WeakTermEnumElim (e, t) les -> do
      let xs = varWeakTerm t
      let ys = varWeakTerm e
      let zs = S.unions $ map (varWeakTerm . snd) les
      S.unions [xs, ys, zs]
    _ :< WeakTermQuestion e t -> do
      let set1 = varWeakTerm e
      let set2 = varWeakTerm t
      S.union set1 set2
    _ :< WeakTermMagic der ->
      foldMap varWeakTerm der
    _ :< WeakTermMatch mSubject (e, t) patList -> do
      let xs1 = S.unions $ map varWeakTerm $ maybeToList mSubject
      let xs2 = varWeakTerm e
      let xs3 = varWeakTerm t
      let xs4 = S.unions $ map (\((_, _, xts), body) -> varWeakTerm' xts [body]) patList
      S.unions [xs1, xs2, xs3, xs4]
    _ :< WeakTermNoema s e ->
      S.unions [varWeakTerm s, varWeakTerm e]
    _ :< WeakTermNoemaIntro s e ->
      S.insert s $ varWeakTerm e
    _ :< WeakTermNoemaElim s e ->
      S.filter (/= s) $ varWeakTerm e
    _ :< WeakTermArray len elemType ->
      S.unions [varWeakTerm len, varWeakTerm elemType]
    _ :< WeakTermArrayIntro elemType elems ->
      S.unions $ varWeakTerm elemType : map varWeakTerm elems
    _ :< WeakTermArrayAccess subject elemType array index ->
      S.unions $ map varWeakTerm [subject, elemType, array, index]

varWeakTerm' :: [BinderF WeakTerm] -> [WeakTerm] -> S.Set Ident
varWeakTerm' binder es =
  case binder of
    [] ->
      S.unions $ map varWeakTerm es
    ((_, x, t) : xts) -> do
      let hs1 = varWeakTerm t
      let hs2 = varWeakTerm' xts es
      S.union hs1 $ S.filter (/= x) hs2

asterWeakTerm :: WeakTerm -> S.Set Int
asterWeakTerm term =
  case term of
    _ :< WeakTermTau ->
      S.empty
    _ :< WeakTermVar {} ->
      S.empty
    _ :< WeakTermVarGlobal {} ->
      S.empty
    _ :< WeakTermPi xts t ->
      asterWeakTerm' xts [t]
    _ :< WeakTermPiIntro _ xts e ->
      asterWeakTerm' xts [e]
    _ :< WeakTermPiElim e es ->
      S.unions $ map asterWeakTerm $ e : es
    _ :< WeakTermSigma xts ->
      asterWeakTerm' xts []
    _ :< WeakTermSigmaIntro es ->
      S.unions $ map asterWeakTerm es
    _ :< WeakTermSigmaElim xts e1 e2 -> do
      let set1 = asterWeakTerm e1
      let set2 = asterWeakTerm' xts [e2]
      S.union set1 set2
    _ :< WeakTermAster h ->
      S.singleton h
    _ :< WeakTermConst _ ->
      S.empty
    _ :< WeakTermInt t _ ->
      asterWeakTerm t
    _ :< WeakTermFloat t _ ->
      asterWeakTerm t
    _ :< WeakTermEnum _ ->
      S.empty
    _ :< WeakTermEnumIntro _ ->
      S.empty
    _ :< WeakTermEnumElim (e, t) les -> do
      let set1 = asterWeakTerm e
      let set2 = asterWeakTerm t
      let set3 = S.unions $ map (\(_, body) -> asterWeakTerm body) les
      S.unions [set1, set2, set3]
    _ :< WeakTermQuestion e t -> do
      let set1 = asterWeakTerm e
      let set2 = asterWeakTerm t
      S.union set1 set2
    _ :< WeakTermMagic der ->
      foldMap asterWeakTerm der
    _ :< WeakTermMatch mSubject (e, t) patList -> do
      let xs1 = S.unions $ map asterWeakTerm $ maybeToList mSubject
      let xs2 = asterWeakTerm e
      let xs3 = asterWeakTerm t
      let xs4 = S.unions $ map (\((_, _, xts), body) -> asterWeakTerm' xts [body]) patList
      S.unions [xs1, xs2, xs3, xs4]
    _ :< WeakTermNoema s e ->
      S.unions [asterWeakTerm s, asterWeakTerm e]
    _ :< WeakTermNoemaIntro _ e ->
      asterWeakTerm e
    _ :< WeakTermNoemaElim _ e ->
      asterWeakTerm e
    _ :< WeakTermArray len elemType ->
      S.unions [asterWeakTerm len, asterWeakTerm elemType]
    _ :< WeakTermArrayIntro elemType elems ->
      S.unions $ asterWeakTerm elemType : map asterWeakTerm elems
    _ :< WeakTermArrayAccess subject elemType array index ->
      S.unions $ map asterWeakTerm [subject, elemType, array, index]

asterWeakTerm' :: [BinderF WeakTerm] -> [WeakTerm] -> S.Set Int
asterWeakTerm' binder es =
  case binder of
    [] ->
      S.unions $ map asterWeakTerm es
    ((_, _, t) : xts) -> do
      let set1 = asterWeakTerm t
      let set2 = asterWeakTerm' xts es
      S.union set1 set2

metaOf :: WeakTerm -> Hint
metaOf (m :< _) =
  m

asVar :: WeakTerm -> Maybe Ident
asVar term =
  case term of
    (_ :< WeakTermVar x) ->
      Just x
    _ ->
      Nothing

toText :: WeakTerm -> T.Text
toText term =
  case term of
    _ :< WeakTermTau ->
      "tau"
    _ :< WeakTermVar x ->
      showVariable x
    _ :< WeakTermVarGlobal x ->
      x
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
    _ :< WeakTermSigmaIntro {} ->
      "<sigma-intro>"
    _ :< WeakTermSigmaElim {} ->
      "<sigma-elim>"
    _ :< WeakTermConst x ->
      x
    _ :< WeakTermAster i ->
      "?M" <> T.pack (show i)
    _ :< WeakTermInt _ a ->
      T.pack $ show a
    _ :< WeakTermFloat _ a ->
      T.pack $ show a
    _ :< WeakTermEnum l ->
      l
    _ :< WeakTermEnumIntro v ->
      v
    _ :< WeakTermEnumElim (e, _) mles -> do
      showCons ["switch", toText e, showItems (map showClause mles)]
    _ :< WeakTermQuestion e _ ->
      toText e
    _ :< WeakTermMagic _ -> do
      "<magic>"
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
      showCons ["noema-intro", asText s, toText e]
    _ :< WeakTermNoemaElim s e ->
      showCons ["noema-elim", asText s, toText e]
    _ :< WeakTermArray len elemType ->
      showCons ["array", toText len, toText elemType]
    _ :< WeakTermArrayIntro elemType elems ->
      showCons $ "array-new" : toText elemType : map toText elems
    _ :< WeakTermArrayAccess subject elemType array index ->
      showCons ["array-access", toText subject, toText elemType, toText array, toText index]

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
showVariable = asText

showCaseClause :: (PatternF WeakTerm, WeakTerm) -> T.Text
showCaseClause (pat, e) =
  inParen $ showPattern pat <> " " <> toText e

showPattern :: (Hint, T.Text, [BinderF WeakTerm]) -> T.Text
showPattern (_, f, xts) = do
  case xts of
    [] ->
      inParen f
    _ -> do
      let xs = map (\(_, x, _) -> x) xts
      inParen $ f <> " " <> T.intercalate " " (map showVariable xs)

showClause :: (EnumCase, WeakTerm) -> T.Text
showClause (c, e) =
  inParen $ showCase c <> " " <> toText e

showCase :: EnumCase -> T.Text
showCase c =
  case c of
    _ :< EnumCaseLabel l ->
      l
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
