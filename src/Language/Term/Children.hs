module Language.Term.Children
  ( termChildren,
    decisionTreeTerms,
  )
where

import Data.Foldable (toList)
import Language.Common.DecisionTree qualified as DT
import Language.Term.Term qualified as TM

termChildren :: TM.TermF TM.Term -> [TM.Term]
termChildren node = do
  case node of
    TM.Var {} ->
      []
    TM.VarGlobal {} ->
      []
    TM.PiIntro _ _ _ defaultArgs body ->
      map snd defaultArgs ++ [body]
    TM.PiElim _ _ callee _ args defaults ->
      callee : args ++ foldMap toList defaults
    TM.DataIntro _ _ _ args ->
      args
    TM.DataElim _ _ oets tree ->
      map (\(_, e, _) -> e) oets ++ decisionTreeTerms tree
    TM.BoxIntro _ letSeq body ->
      map snd letSeq ++ [body]
    TM.BoxIntroLift _ child ->
      [child]
    TM.EmbedIntro child ->
      [child]
    TM.BoxElim _ castSeq _ value uncastSeq body ->
      map snd castSeq ++ [value] ++ map snd uncastSeq ++ [body]
    TM.CodeIntro body ->
      [body]
    TM.CodeElim _ body ->
      [body]
    TM.TauIntro _ ->
      []
    TM.TauElim _ _ value body ->
      [value, body]
    TM.Let _ value body ->
      [value, body]
    TM.Invoke _ body ->
      [body]
    TM.Prim _ ->
      []
    TM.Magic _ magic ->
      toList magic

decisionTreeTerms :: DT.DecisionTree t TM.Term -> [TM.Term]
decisionTreeTerms tree = do
  case tree of
    DT.Leaf _ letSeq body ->
      map snd letSeq ++ [body]
    DT.Unreachable ->
      []
    DT.Switch _ (fallback, cases) ->
      decisionTreeTerms fallback ++ foldMap caseTerms cases

caseTerms :: DT.Case t TM.Term -> [TM.Term]
caseTerms decisionCase = do
  case decisionCase of
    DT.LiteralCase _ _ cont ->
      decisionTreeTerms cont
    DT.ConsCase record ->
      decisionTreeTerms $ DT.cont record
