module Entity.Term.Weaken
  ( weaken,
    weakenBinder,
  )
where

import Control.Comonad.Cofree
import Entity.Hint
import Entity.Ident
import Entity.LamKind
import Entity.PrimNum
import Entity.Term
import Entity.Term.FromPrimNum
import Entity.WeakTerm

weaken :: Term -> WeakTerm
weaken term =
  case term of
    m :< TermTau ->
      m :< WeakTermTau
    m :< TermVar x ->
      m :< WeakTermVar x
    m :< TermVarGlobal g arity ->
      m :< WeakTermVarGlobal g arity
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
    m :< TermSigma xts ->
      m :< WeakTermSigma (map weakenBinder xts)
    m :< TermSigmaIntro es ->
      m :< WeakTermSigmaIntro (map weaken es)
    m :< TermSigmaElim xts e1 e2 -> do
      m :< WeakTermSigmaElim (map weakenBinder xts) (weaken e1) (weaken e2)
    m :< TermLet mxt e1 e2 ->
      m :< WeakTermLet (weakenBinder mxt) (weaken e1) (weaken e2)
    m :< TermPrim x ->
      m :< WeakTermPrim x
    m :< TermInt size x ->
      m :< WeakTermInt (weaken $ fromPrimNum m (PrimNumInt size)) x
    m :< TermFloat size x ->
      m :< WeakTermFloat (weaken $ fromPrimNum m (PrimNumFloat size)) x
    m :< TermEnum x ->
      m :< WeakTermEnum x
    m :< TermEnumIntro label ->
      m :< WeakTermEnumIntro label
    m :< TermEnumElim (e, t) branchList -> do
      let t' = weaken t
      let e' = weaken e
      let (caseList, es) = unzip branchList
      -- let caseList' = map (\(me, ec) -> (me, weakenEnumCase ec)) caseList
      let es' = map weaken es
      m :< WeakTermEnumElim (e', t') (zip caseList es')
    m :< TermMagic der -> do
      m :< WeakTermMagic (fmap weaken der)
    m :< TermMatch (e, t) patList -> do
      let e' = weaken e
      let t' = weaken t
      let patList' = map (\((mp, p, arity, xts), body) -> ((mp, p, arity, map weakenBinder xts), weaken body)) patList
      m :< WeakTermMatch (e', t') patList'

weakenBinder :: (Hint, Ident, Term) -> (Hint, Ident, WeakTerm)
weakenBinder (m, x, t) =
  (m, x, weaken t)

weakenKind :: LamKindF Term -> LamKindF WeakTerm
weakenKind kind =
  case kind of
    LamKindNormal ->
      LamKindNormal
    LamKindCons dataName consName consNumber dataType ->
      LamKindCons dataName consName consNumber (weaken dataType)
    LamKindFix xt ->
      LamKindFix (weakenBinder xt)
