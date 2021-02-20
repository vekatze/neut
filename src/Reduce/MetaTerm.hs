module Reduce.MetaTerm (reduceMetaTerm) where

import Control.Monad.IO.Class
import Data.EnumCase
import Data.Env
import Data.Hint
import Data.Ident
import Data.Int
import qualified Data.IntMap as IntMap
import Data.MetaTerm
import qualified Data.Text as T
import Data.Tree

reduceMetaTerm :: MetaTermPlus -> WithEnv MetaTermPlus
reduceMetaTerm term =
  case term of
    (m, MetaTermImpElim e es) -> do
      e' <- reduceMetaTerm e
      es' <- mapM reduceMetaTerm es
      case e' of
        (mLam, MetaTermImpIntro xs mRest body) -> do
          h <- newNameWith' "SELF"
          reduceFix (mLam, MetaTermFix h xs mRest body) es'
        (_, MetaTermFix {}) ->
          reduceFix e' es'
        (_, MetaTermConst c) ->
          reduceConstApp m c es'
        _ ->
          raiseError m "encountered an ill-typed application"
    (m, MetaTermEnumElim (e, _) caseList) -> do
      e' <- reduceMetaTerm e
      let caseList' = map (\(c, body) -> (snd c, body)) caseList
      case e' of
        (_, MetaTermEnumIntro l) ->
          case lookup (EnumCaseLabel l) caseList' of
            Just body ->
              reduceMetaTerm body
            Nothing ->
              raiseError m "found an ill-typed switch"
        _ -> do
          raiseError m "found an ill-typed switch"
    (m, MetaTermNode es) -> do
      es' <- mapM reduceMetaTerm es
      return (m, MetaTermNode es')
    _ -> do
      return term

reduceFix :: MetaTermPlus -> [MetaTermPlus] -> WithEnv MetaTermPlus
reduceFix e es =
  case e of
    (m, MetaTermFix f xs mRest body)
      | Just rest <- mRest -> do
        p "reduce-fix-with-rest"
        if length xs > length es
          then raiseError m "arity mismatch"
          else do
            let es1 = take (length xs) es
            -- let es2 = drop (length xs) es
            -- let es2 = map (\x -> (m, MetaTermNecElim x)) $ drop (length xs) es
            let restArg = (m, MetaTermNode (drop (length xs) es))
            -- let restArg = (m, MetaTermNecIntro (m, MetaTermNode es2))
            let sub = IntMap.fromList $ (asInt f, e) : zip (map asInt xs) es1 ++ [(asInt rest, restArg)]
            reduceMetaTerm $ substMetaTerm sub body
      | otherwise -> do
        if length xs /= length es
          then raiseError m "arity mismatch"
          else do
            let sub = IntMap.fromList $ (asInt f, e) : zip (map asInt xs) es
            reduceMetaTerm $ substMetaTerm sub body
    _ ->
      raiseCritical (fst e) "unreachable"

reduceConstApp :: Hint -> T.Text -> [MetaTermPlus] -> WithEnv MetaTermPlus
reduceConstApp m c es =
  case c of
    "cons"
      | [t, (_, MetaTermNode ts)] <- es ->
        return (m, MetaTermNode (t : ts))
    "dump"
      | [arg] <- es -> do
        liftIO $ putStrLn $ T.unpack $ showAsSExp $ toTree arg
        return (m, MetaTermEnumIntro "top.unit")
    "head"
      | [(_, MetaTermNode (h : _))] <- es ->
        return h
    "is-nil"
      | [(_, MetaTermNode ts)] <- es ->
        return $ liftBool (null ts) m
    "leaf-mul"
      | [(mLeaf, MetaTermLeaf s1), (_, MetaTermLeaf s2)] <- es ->
        return (mLeaf, MetaTermLeaf (s1 <> s2))
    "leaf-equal"
      | [(_, MetaTermLeaf s1), (_, MetaTermLeaf s2)] <- es ->
        return $ liftBool (s1 == s2) m
    "tail"
      | [(mNode, MetaTermNode (_ : rest))] <- es ->
        return (mNode, MetaTermNode rest)
    _
      | Just op <- toArithOp c,
        [(_, MetaTermInt64 i1), (_, MetaTermInt64 i2)] <- es ->
        return (m, MetaTermInt64 (op i1 i2))
      | Just op <- toCmpOp c,
        [(_, MetaTermInt64 i1), (_, MetaTermInt64 i2)] <- es ->
        return $ liftBool (op i1 i2) m
      | otherwise -> do
        let textList = map (showAsSExp . toTree) es
        raiseError m $ "the constant `" <> c <> "` cannot be used with the following arguments:\n" <> T.intercalate "\n" textList

toArithOp :: T.Text -> Maybe (Int64 -> Int64 -> Int64)
toArithOp opStr =
  case opStr of
    "int-add" ->
      Just (+)
    "int-sub" ->
      Just (-)
    "int-mul" ->
      Just (*)
    "int-div" ->
      Just div
    _ ->
      Nothing

toCmpOp :: T.Text -> Maybe (Int64 -> Int64 -> Bool)
toCmpOp opStr =
  case opStr of
    "int-gt" ->
      Just (<)
    "int-ge" ->
      Just (<=)
    "int-lt" ->
      Just (>)
    "int-le" ->
      Just (>=)
    "int-eq" ->
      Just (==)
    _ ->
      Nothing

liftBool :: Bool -> Hint -> MetaTermPlus
liftBool b m =
  if b
    then (m, MetaTermEnumIntro "bool.true")
    else (m, MetaTermEnumIntro "bool.false")
