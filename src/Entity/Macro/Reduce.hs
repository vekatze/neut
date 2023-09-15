module Entity.Macro.Reduce (reduce) where

import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.Atom qualified as AT
import Entity.Error
import Entity.Hint
import Entity.Macro
import Entity.Tree

data Axis = Axis
  { rules :: Rules,
    steps :: Int,
    maxSteps :: Int
  }

reduce :: Int -> Rules -> Tree -> EE Tree
reduce maxSteps rules =
  reduce' (Axis {rules = rules, steps = 0, maxSteps = maxSteps})

inc :: Axis -> Axis
inc ax =
  ax {steps = 1 + steps ax}

reduce' :: Axis -> Tree -> EE Tree
reduce' axis tree@(m :< _) =
  if steps axis >= maxSteps axis
    then
      Left $
        newError m $
          "detected a possible infinite loop (tried "
            <> T.pack (show (maxSteps axis))
            <> " steps)"
    else do
      case tree of
        _ :< Atom _ ->
          return tree
        _ :< Node [_ :< Atom (AT.Symbol "cons!"), car, cdr] -> do
          car' <- reduce' axis car
          cdr' <- reduce' axis cdr
          (_, ts) <- toNode cdr'
          return $ m :< Node (car' : ts)
        _ :< Node ts -> do
          ts' <- mapM (reduce' (inc axis)) ts
          case ts' of
            t : rest
              | _ :< Atom (AT.Symbol sym) <- t,
                Just cands <- Map.lookup sym (rules axis),
                Just (sub, body) <- findRule m cands rest -> do
                  reduce' (inc axis) $ subst m sub body
            _ ->
              return $ m :< Node ts'
        _ :< List ts -> do
          ts' <- mapM (reduce' axis) ts
          return $ m :< List ts'

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
        Var macroVar -> do
          sub <- macroMatch m (macroRemArgs, mVariadic) remArgs
          return $ Map.insert macroVar arg sub
        ArgNode macroTrees
          | _ :< Node trees <- arg -> do
              sub1 <- macroMatch m macroTrees trees
              sub2 <- macroMatch m (macroRemArgs, mVariadic) remArgs
              return $ Map.union sub1 sub2
        ArgList macroTrees
          | _ :< List trees <- arg -> do
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
    _ :< List ts -> do
      let ts' = map (subst m sub) ts
      m :< List ts'
