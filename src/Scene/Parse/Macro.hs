module Scene.Parse.Macro (interpretDefineMacro) where

import Context.App
import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Data.Text qualified as T
import Entity.Atom qualified as AT
import Entity.BaseName (fromTextOptional)
import Entity.Error
import Entity.Macro
import Entity.RawIdent (RawIdent)
import Entity.Tree

interpretDefineMacro :: Tree -> App MacroInfo
interpretDefineMacro t = do
  case t of
    m :< Node ts -> do
      let (ts', _) = splitAttrs ts
      case ts' of
        (def : name : clauses) -> do
          Throw.liftEither $ chunk "rule" def
          (_, name') <- Throw.liftEither $ getSymbol name >>= fromTextOptional
          clauses' <- Throw.liftEither $ mapM reflClause clauses
          name'' <- Locator.attachCurrentLocator name'
          return (name'', clauses')
        _ ->
          Throw.raiseError m "rule"
    m :< _ ->
      Throw.raiseError m "rule"

reflClause :: Tree -> EE (Args, Tree)
reflClause t = do
  (argTrees, body) <- splitClause t
  return (reflArgs argTrees, body)

splitClause :: Tree -> EE ([Tree], Tree)
splitClause t = do
  (m, ts) <- toNode t
  reflArrowArgs' m ts

reflArgs :: [Tree] -> Args
reflArgs argTrees =
  case argTrees of
    [] ->
      ([], Nothing)
    [t] ->
      case getRestArg t of
        Just sym ->
          ([], Just sym)
        Nothing -> do
          ([reflArg t], Nothing)
    t : ts -> do
      let (args, restArg) = reflArgs ts
      (reflArg t : args, restArg)

reflArg :: Tree -> Arg
reflArg t =
  case t of
    _ :< Atom at ->
      case at of
        AT.Symbol s ->
          case getLiteralSymbol s of
            Just literalSymbol ->
              Literal literalSymbol
            Nothing ->
              Var s
        AT.String str ->
          Str str
        AT.DefiniteDescription dd ->
          DefiniteDescription dd
    _ :< Node ts -> do
      ArgNode (reflArgs ts)
    _ :< List ts -> do
      ArgList (reflArgs ts)

getRestArg :: Tree -> Maybe RawIdent
getRestArg t =
  case t of
    _ :< Atom (AT.Symbol sym)
      | T.isPrefixOf "*" sym ->
          return sym
    _ ->
      Nothing

getLiteralSymbol :: T.Text -> Maybe T.Text
getLiteralSymbol text =
  case T.uncons text of
    Just ('\'', rest) ->
      return rest
    _ ->
      Nothing
