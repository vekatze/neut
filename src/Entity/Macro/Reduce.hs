module Entity.Macro.Reduce (reduce) where

import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Debug.Trace
import Entity.Atom qualified as AT
import Entity.Hint
import Entity.Macro
import Entity.Tree

reduce :: Rules -> Tree -> Tree
reduce rules tree =
  case tree of
    _ :< Atom _ ->
      tree
    m :< Node ts -> do
      let ts' = map (reduce rules) $ resolveSplice ts
      case ts' of
        t : rest
          | _ :< Atom (AT.Symbol sym) <- t,
            Just cands <- Map.lookup sym rules,
            Just (sub, body) <- findRule m cands rest -> do
              reduce rules $ trace ("body-after: " <> T.unpack (showTree body)) $ subst m sub body
        _ ->
          m :< Node ts'
    m :< List tss ->
      m :< List (map (map (reduce rules) . resolveSplice) tss)

findRule :: Hint -> [(Args, Tree)] -> [Tree] -> Maybe (Sub, Tree)
findRule m cands args =
  case cands of
    [] ->
      Nothing
    (macroArgs, macroBody) : rest -> do
      case macroMatch m macroArgs args of
        Just sub -> do
          _ <- trace ("found subs for: " <> show (Map.keys sub)) $ return ()
          return (sub, macroBody)
        Nothing ->
          findRule m rest args

macroMatch :: Hint -> Args -> [Tree] -> Maybe Sub
macroMatch m macroArgs args = do
  _ <- trace ("macroArgs: " <> show macroArgs) $ return ()
  _ <- trace ("args: " <> show (map showTree args)) $ return ()
  case (macroArgs, args) of
    (([], mVariadic), []) ->
      case mVariadic of
        Nothing ->
          return Map.empty
        Just variadic ->
          return $ Map.singleton variadic (toSpliceTree m args)
    (([], mVariadic), _) ->
      case mVariadic of
        Nothing ->
          Nothing
        Just variadic ->
          return $ Map.singleton variadic (toSpliceTree m args)
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

toSpliceTree :: Hint -> [Tree] -> Tree
toSpliceTree m ts =
  m :< Node [m :< Atom (AT.Symbol "splice"), m :< Node ts]

resolveSplice :: [Tree] -> [Tree]
resolveSplice =
  expandSplice . map findSplice

findSplice :: Tree -> Either Tree [Tree]
findSplice t =
  case t of
    _ :< Node [_ :< Atom (AT.Symbol "splice"), _ :< Node ts] ->
      Right ts
    _ ->
      Left t

expandSplice :: [Either Tree [Tree]] -> [Tree]
expandSplice treeSeq =
  case treeSeq of
    [] ->
      []
    Left t : rest ->
      t : expandSplice rest
    Right ts : rest ->
      ts ++ expandSplice rest
