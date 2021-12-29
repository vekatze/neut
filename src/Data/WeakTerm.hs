{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.WeakTerm where

import Control.Comonad.Cofree (Cofree (..))
import Data.Basic
  ( EnumCase,
    EnumCaseF (..),
    Hint,
    Ident (..),
    LamKind (LamKindCons, LamKindFix),
    Opacity,
    asText,
    fromLamKind,
    isOpaque,
    isTransparent,
  )
import Data.Binary (Binary)
import qualified Data.IntMap as IntMap
import Data.LowType (Derangement, showIntSize)
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.PQueue.Min as Q
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics (Generic)

data WeakTermF a
  = WeakTermTau
  | WeakTermVar Ident
  | WeakTermVarGlobal T.Text
  | WeakTermPi [WeakBinderF a] a
  | WeakTermPiIntro Opacity (LamKind (WeakBinderF a)) [WeakBinderF a] a
  | WeakTermPiElim a [a]
  | WeakTermAster Int
  | WeakTermConst T.Text
  | WeakTermInt a Integer
  | WeakTermFloat a Double
  | WeakTermEnum T.Text
  | WeakTermEnumIntro T.Text
  | WeakTermEnumElim (a, a) [(EnumCase, a)]
  | WeakTermQuestion a a -- e : t (output the type `t` as note)
  | WeakTermDerangement Derangement [a] -- (derangement kind arg-1 ... arg-n)
  | WeakTermCase
      a -- result type
      (Maybe a) -- noetic subject (this is for `case-noetic`)
      (a, a) -- (pattern-matched value, its type)
      [(WeakPatternF a, a)]
  | WeakTermIgnore a
  deriving (Generic)

instance (Binary a) => Binary (WeakTermF a)

type WeakBinderF a =
  (Hint, Ident, a)

type WeakPatternF a =
  (Hint, T.Text, [WeakBinderF a])

type WeakTerm = Cofree WeakTermF Hint

type WeakBinder = WeakBinderF WeakTerm

type WeakPattern = WeakPatternF WeakTerm

instance Binary WeakTerm

type SubstWeakTerm =
  IntMap.IntMap WeakTerm

type WeakText =
  (Hint, T.Text, WeakTerm)

type Def =
  (Hint, WeakBinder, [WeakBinder], WeakTerm)

type IdentDef =
  (Ident, Def)

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
    _ :< WeakTermPiIntro _ k xts e ->
      varWeakTerm' (catMaybes [fromLamKind k] ++ xts) [e]
    _ :< WeakTermPiElim e es -> do
      let xs = varWeakTerm e
      let ys = S.unions $ map varWeakTerm es
      S.union xs ys
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
    _ :< WeakTermDerangement _ es ->
      S.unions $ map varWeakTerm es
    _ :< WeakTermCase resultType mSubject (e, t) patList -> do
      let xs1 = varWeakTerm resultType
      let xs2 = S.unions $ map varWeakTerm $ maybeToList mSubject
      let xs3 = varWeakTerm e
      let xs4 = varWeakTerm t
      let xs5 = S.unions $ map (\((_, _, xts), body) -> varWeakTerm' xts [body]) patList
      S.unions [xs1, xs2, xs3, xs4, xs5]
    _ :< WeakTermIgnore e ->
      varWeakTerm e

varWeakTerm' :: [WeakBinder] -> [WeakTerm] -> S.Set Ident
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
      asterWeakTerm' xts t
    _ :< WeakTermPiIntro _ _ xts e ->
      asterWeakTerm' xts e
    _ :< WeakTermPiElim e es ->
      S.unions $ map asterWeakTerm $ e : es
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
    _ :< WeakTermDerangement _ es ->
      S.unions $ map asterWeakTerm es
    _ :< WeakTermCase resultType mSubject (e, t) patList -> do
      let xs1 = asterWeakTerm resultType
      let xs2 = S.unions $ map asterWeakTerm $ maybeToList mSubject
      let xs3 = asterWeakTerm e
      let xs4 = asterWeakTerm t
      let xs5 = S.unions $ map (\((_, _, xts), body) -> asterWeakTerm' xts body) patList
      S.unions [xs1, xs2, xs3, xs4, xs5]
    _ :< WeakTermIgnore e ->
      asterWeakTerm e

asterWeakTerm' :: [WeakBinder] -> WeakTerm -> S.Set Int
asterWeakTerm' binder e =
  case binder of
    [] ->
      asterWeakTerm e
    ((_, _, t) : xts) -> do
      let set1 = asterWeakTerm t
      let set2 = asterWeakTerm' xts e
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
    _ :< WeakTermPiIntro opacity kind xts e -> do
      case kind of
        LamKindFix (_, x, _) -> do
          let argStr = inParen $ showItems $ map showArg xts
          if isOpaque opacity
            then showCons ["fix-irreducible", showVariable x, argStr, toText e]
            else showCons ["fix", showVariable x, argStr, toText e]
        LamKindCons _ _ -> do
          let argStr = inParen $ showItems $ map showArg xts
          if isTransparent opacity
            then showCons ["λ", argStr, toText e]
            else showCons ["λ-irreducible", argStr, toText e]
        -- "<cons>"
        _ -> do
          let argStr = inParen $ showItems $ map showArg xts
          if isTransparent opacity
            then showCons ["λ", argStr, toText e]
            else showCons ["λ-irreducible", argStr, toText e]
    _ :< WeakTermPiElim e es ->
      case e of
        -- (_, WeakTermAster _) ->
        --   "*"
        _ ->
          showCons $ map toText $ e : es
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
    _ :< WeakTermDerangement i es -> do
      let es' = map toText es
      showCons $ "derangement" : T.pack (show i) : es'
    _ :< WeakTermCase _ mSubject (e, _) caseClause -> do
      case mSubject of
        Nothing -> do
          showCons $ "case" : toText e : map showCaseClause caseClause
        Just _ -> do
          showCons $ "case-noetic" : toText e : map showCaseClause caseClause
    _ :< WeakTermIgnore e ->
      showCons ["ignore", toText e]

inParen :: T.Text -> T.Text
inParen s =
  "(" <> s <> ")"

showArg :: (Hint, Ident, WeakTerm) -> T.Text
showArg (_, x, t) =
  inParen $ showVariable x <> " " <> toText t

showTypeArgs :: [WeakBinder] -> T.Text
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

showCaseClause :: (WeakPattern, WeakTerm) -> T.Text
showCaseClause (pat, e) =
  inParen $ showPattern pat <> " " <> toText e

showPattern :: (Hint, T.Text, [WeakBinder]) -> T.Text
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
