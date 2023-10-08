module Scene.Parse.Articulate (articulate) where

import Context.App
import Control.Comonad.Cofree
import Entity.Atom qualified as AT
import Entity.DefiniteDescription qualified as DD
import Entity.Name qualified as N
import Entity.Tree
import Scene.Parse.Discern.Name

articulate :: Tree -> App Tree
articulate tree =
  case tree of
    m :< Atom at ->
      case at of
        AT.Symbol sym -> do
          case DD.getLocatorPair m sym of
            Left _ -> do
              result <- resolveNameOrError m (N.Var sym)
              case result of
                Left {} ->
                  return tree
                Right (dd, _) ->
                  return $ m :< Atom (AT.DefiniteDescription dd)
            Right locator -> do
              result <- resolveLocatorOrError m locator
              case result of
                Left {} ->
                  return tree
                Right (dd, _) ->
                  return $ m :< Atom (AT.DefiniteDescription dd)
        AT.String {} ->
          return tree
        AT.DefiniteDescription {} ->
          return tree
    m :< Node ts -> do
      ts' <- mapM articulate ts
      return $ m :< Node ts'
    m :< List ts -> do
      ts' <- mapM articulate ts
      return $ m :< List ts'
