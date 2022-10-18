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
import qualified Entity.WeakTerm as WT

weaken :: Term -> WT.WeakTerm
weaken term =
  case term of
    m :< TermTau ->
      m :< WT.Tau
    m :< TermVar x ->
      m :< WT.Var x
    m :< TermVarGlobal g arity ->
      m :< WT.VarGlobal g arity
    m :< TermPi xts t ->
      m :< WT.Pi (map weakenBinder xts) (weaken t)
    m :< TermPiIntro kind xts e -> do
      let kind' = weakenKind kind
      let xts' = map weakenBinder xts
      let e' = weaken e
      m :< WT.PiIntro kind' xts' e'
    m :< TermPiElim e es -> do
      let e' = weaken e
      let es' = map weaken es
      m :< WT.PiElim e' es'
    m :< TermSigma xts ->
      m :< WT.Sigma (map weakenBinder xts)
    m :< TermSigmaIntro es ->
      m :< WT.SigmaIntro (map weaken es)
    m :< TermSigmaElim xts e1 e2 -> do
      m :< WT.SigmaElim (map weakenBinder xts) (weaken e1) (weaken e2)
    m :< TermLet mxt e1 e2 ->
      m :< WT.Let (weakenBinder mxt) (weaken e1) (weaken e2)
    m :< TermPrim x ->
      m :< WT.Prim x
    m :< TermInt size x ->
      m :< WT.Int (weaken $ fromPrimNum m (PrimNumInt size)) x
    m :< TermFloat size x ->
      m :< WT.Float (weaken $ fromPrimNum m (PrimNumFloat size)) x
    m :< TermEnum x ->
      m :< WT.Enum x
    m :< TermEnumIntro label ->
      m :< WT.EnumIntro label
    m :< TermEnumElim (e, t) branchList -> do
      let t' = weaken t
      let e' = weaken e
      let (caseList, es) = unzip branchList
      -- let caseList' = map (\(me, ec) -> (me, weakenEnumCase ec)) caseList
      let es' = map weaken es
      m :< WT.EnumElim (e', t') (zip caseList es')
    m :< TermMagic der -> do
      m :< WT.Magic (fmap weaken der)
    m :< TermMatch (e, t) patList -> do
      let e' = weaken e
      let t' = weaken t
      let patList' = map (\((mp, p, arity, xts), body) -> ((mp, p, arity, map weakenBinder xts), weaken body)) patList
      m :< WT.Match (e', t') patList'

weakenBinder :: (Hint, Ident, Term) -> (Hint, Ident, WT.WeakTerm)
weakenBinder (m, x, t) =
  (m, x, weaken t)

weakenKind :: LamKindF Term -> LamKindF WT.WeakTerm
weakenKind kind =
  case kind of
    LamKindNormal ->
      LamKindNormal
    LamKindCons dataName consName consNumber dataType ->
      LamKindCons dataName consName consNumber (weaken dataType)
    LamKindFix xt ->
      LamKindFix (weakenBinder xt)
