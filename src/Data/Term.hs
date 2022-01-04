{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Term where

import Control.Comonad.Cofree (Cofree (..))
import Control.Exception.Safe (MonadThrow)
import Data.Basic
  ( BinderF,
    EnumCase,
    Hint,
    Ident,
    LamKindF (..),
    PatternF,
  )
import Data.Binary (Binary)
import qualified Data.IntMap as IntMap
import Data.Log (raiseCritical)
import Data.LowType
  ( Derangement,
    FloatSize,
    IntSize,
    LowType (LowTypeFloat, LowTypeInt),
    showFloatSize,
    showIntSize,
  )
import qualified Data.Text as T
import Data.WeakTerm
  ( WeakTerm,
    WeakTermF
      ( WeakTermConst,
        WeakTermDerangement,
        WeakTermEnum,
        WeakTermEnumElim,
        WeakTermEnumIntro,
        WeakTermFloat,
        WeakTermIgnore,
        WeakTermInt,
        WeakTermMatch,
        WeakTermPi,
        WeakTermPiElim,
        WeakTermPiIntro,
        WeakTermTau,
        WeakTermVar,
        WeakTermVarGlobal
      ),
  )
import GHC.Generics (Generic)

data TermF a
  = TermTau
  | TermVar Ident
  | TermVarGlobal T.Text
  | TermPi [BinderF a] a
  | TermPiIntro (LamKindF a) [BinderF a] a
  | TermPiElim a [a]
  | TermConst T.Text
  | TermInt IntSize Integer
  | TermFloat FloatSize Double
  | TermEnum T.Text
  | TermEnumIntro T.Text
  | TermEnumElim (a, a) [(EnumCase, a)]
  | TermDerangement Derangement [a]
  | TermMatch
      a -- result type
      (Maybe a) -- noetic subject (this is for `case-noetic`)
      (a, a) -- (pattern-matched value, its type)
      [(PatternF a, a)]
  | TermIgnore a
  deriving (Show, Generic)

type Term = Cofree TermF Hint

instance (Binary a) => Binary (TermF a)

instance Binary Term

type SubstTerm =
  IntMap.IntMap Term

type TypeEnv =
  IntMap.IntMap Term

asVar :: Term -> Maybe Ident
asVar term =
  case term of
    _ :< TermVar x ->
      Just x
    _ ->
      Nothing

weaken :: Term -> WeakTerm
weaken term =
  case term of
    m :< TermTau ->
      m :< WeakTermTau
    m :< TermVar x ->
      m :< WeakTermVar x
    m :< TermVarGlobal g ->
      m :< WeakTermVarGlobal g
    m :< TermPi xts t ->
      m :< WeakTermPi (map weakenBinder xts) (weaken t)
    m :< TermPiIntro kind xts e -> do
      let kind' = weakenKind kind
      let xts' = map weakenBinder xts
      let e' = weaken e
      m :< WeakTermPiIntro kind' xts' e'
    m :< TermPiElim e es -> do
      let e' = weaken e
      let es' = map weaken es
      m :< WeakTermPiElim e' es'
    m :< TermConst x ->
      m :< WeakTermConst x
    m :< TermInt size x ->
      m :< WeakTermInt (m :< WeakTermConst (showIntSize size)) x
    m :< TermFloat size x ->
      m :< WeakTermFloat (m :< WeakTermConst (showFloatSize size)) x
    m :< TermEnum x ->
      m :< WeakTermEnum x
    m :< TermEnumIntro l ->
      m :< WeakTermEnumIntro l
    m :< TermEnumElim (e, t) branchList -> do
      let t' = weaken t
      let e' = weaken e
      let (caseList, es) = unzip branchList
      -- let caseList' = map (\(me, ec) -> (me, weakenEnumCase ec)) caseList
      let es' = map weaken es
      m :< WeakTermEnumElim (e', t') (zip caseList es')
    m :< TermDerangement i es -> do
      let es' = map weaken es
      m :< WeakTermDerangement i es'
    m :< TermMatch resultType mSubject (e, t) patList -> do
      let resultType' = weaken resultType
      let mSubject' = fmap weaken mSubject
      let e' = weaken e
      let t' = weaken t
      let patList' = map (\((mp, p, xts), body) -> ((mp, p, map weakenBinder xts), weaken body)) patList
      m :< WeakTermMatch resultType' mSubject' (e', t') patList'
    m :< TermIgnore e ->
      m :< WeakTermIgnore (weaken e)

weakenBinder :: (Hint, Ident, Term) -> (Hint, Ident, WeakTerm)
weakenBinder (m, x, t) =
  (m, x, weaken t)

weakenKind :: LamKindF Term -> LamKindF WeakTerm
weakenKind kind =
  case kind of
    LamKindNormal ->
      LamKindNormal
    LamKindCons dataName consName dataType ->
      LamKindCons dataName consName (weaken dataType)
    LamKindFix xt ->
      LamKindFix (weakenBinder xt)
    LamKindResourceHandler ->
      LamKindResourceHandler

lowTypeToType :: (MonadThrow m) => Hint -> LowType -> m Term
lowTypeToType m lowType =
  case lowType of
    LowTypeInt s ->
      return $ m :< TermConst (showIntSize s)
    LowTypeFloat s ->
      return $ m :< TermConst (showFloatSize s)
    _ ->
      raiseCritical m "invalid argument passed to lowTypeToType"
