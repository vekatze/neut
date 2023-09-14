module Scene.Parse.Macro (interpretDefineMacro) where

import Context.App
import Context.Remark (printNote')
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Data.Text qualified as T
import Entity.Atom qualified as AT
import Entity.Error
import Entity.Hint
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
          Throw.liftEither $ chunk "defmacro" def
          (_, name') <- Throw.liftEither $ getSymbol name
          clauses' <- Throw.liftEither $ mapM reflClause clauses
          return (name', clauses')
        _ ->
          Throw.raiseError m "defmacro"
    m :< _ ->
      Throw.raiseError m "defmacro"

reflClause :: Tree -> EE (Args, Tree)
reflClause t = do
  (argTrees, body) <- splitClause t
  args <- reflArgs argTrees
  return (args, body)

splitClause :: Tree -> EE ([Tree], Tree)
splitClause t = do
  (m, ts) <- toNode t
  splitClause' m ts

splitClause' :: Hint -> [Tree] -> EE ([Tree], Tree)
splitClause' m trees =
  case trees of
    [] ->
      Left $ newError m "couldn't find `->`"
    t : ts ->
      case t of
        _ :< Atom (AT.Symbol "->") -> do
          body <- getSingleListElem' m ts
          return ([], body)
        _ -> do
          (args, body) <- splitClause' m ts
          return (t : args, body)

reflArgs :: [Tree] -> EE Args
reflArgs argTrees =
  case argTrees of
    [] ->
      return ([], Nothing)
    [t] ->
      case getRestArg t of
        Just sym ->
          return ([], Just sym)
        Nothing -> do
          t' <- reflArg t
          return ([t'], Nothing)
    t : ts -> do
      (args, restArg) <- reflArgs ts
      t' <- reflArg t
      return (t' : args, restArg)

reflArg :: Tree -> EE Arg
reflArg t =
  case t of
    m :< Atom at ->
      case at of
        AT.Symbol s ->
          case getLiteralSymbol s of
            Just literalSymbol ->
              return $ Literal literalSymbol
            Nothing ->
              return $ Var s
        AT.String _ ->
          Left $ newError m "reflArg against a string"
    _ :< Node ts -> do
      ts' <- reflArgs ts
      return $ ArgNode ts'
    m :< List _ ->
      Left $ newError m "reflArg against a list"

getRestArg :: Tree -> Maybe RawIdent
getRestArg t =
  case t of
    _ :< Atom (AT.Symbol sym)
      | T.isPrefixOf "+" sym ->
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
