module Entity.Macro.Reduce (reduce) where

import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as Map
import Debug.Trace (trace)
import Entity.Atom qualified as AT
import Entity.Error
import Entity.Hint
import Entity.Macro
import Entity.Tree

reduce :: Rules -> Tree -> EE Tree
reduce rules tree =
  case tree of
    _ :< Atom _ ->
      return tree
    m :< Node [_ :< Atom (AT.Symbol "cons!"), car, cdr] -> do
      car' <- reduce rules car
      cdr' <- reduce rules cdr
      (_, ts) <- toNode cdr'
      return $ m :< Node (car' : ts)
    m :< Node ts -> do
      ts' <- mapM (reduce rules) ts
      case ts' of
        t : rest
          | _ :< Atom (AT.Symbol sym) <- t,
            Just cands <- Map.lookup sym rules,
            Just (sub, body) <- findRule m cands rest -> do
              reduce rules $ trace ("sub: " <> show (map showTree (Map.elems sub))) $ subst m sub body
        _ ->
          return $ m :< Node ts'
    m :< List tss -> do
      tss' <- mapM (mapM (reduce rules)) tss
      return $ m :< List tss'

findRule :: Hint -> [(Args, Tree)] -> [Tree] -> Maybe (Sub, Tree)
findRule m cands args =
  case cands of
    [] ->
      Nothing
    (macroArgs, macroBody) : rest -> do
      case macroMatch m macroArgs args of
        Just sub -> do
          return (sub, macroBody)
        Nothing ->
          findRule m rest args

macroMatch :: Hint -> Args -> [Tree] -> Maybe Sub
macroMatch m macroArgs args = do
  case (macroArgs, args) of
    (([], mVariadic), []) ->
      case mVariadic of
        Nothing ->
          return Map.empty
        Just variadic ->
          return $ Map.singleton variadic (m :< Node args)
    (([], mVariadic), _) ->
      case mVariadic of
        Nothing ->
          Nothing
        Just variadic ->
          return $ Map.singleton variadic (m :< Node args)
    ((_ : _, _), []) ->
      Nothing
    ((macroArg : macroRemArgs, mVariadic), arg : remArgs) -> do
      case macroArg of
        Literal lit
          | _ :< Atom (AT.Symbol sym) <- arg,
            lit == sym ->
              macroMatch m (macroRemArgs, mVariadic) remArgs
          | otherwise ->
              Nothing
        Var macroVar -> do
          sub <- macroMatch m (macroRemArgs, mVariadic) remArgs
          return $ Map.insert macroVar arg sub
        ArgNode macroTrees ->
          case arg of
            _ :< Node trees -> do
              sub1 <- macroMatch m macroTrees trees
              sub2 <- macroMatch m (macroRemArgs, mVariadic) remArgs
              return $ Map.union sub1 sub2
            _ ->
              Nothing

subst :: Hint -> Sub -> Tree -> Tree
subst m sub tree =
  case tree of
    _ :< Atom at ->
      case at of
        AT.Symbol sym ->
          case Map.lookup sym sub of
            Just t ->
              t
            Nothing ->
              m :< Atom at
        AT.String {} ->
          m :< Atom at
    _ :< Node ts -> do
      let ts' = map (subst m sub) ts
      m :< Node ts'
    _ :< List tss -> do
      let tss' = map (map (subst m sub)) tss
      m :< List tss'
