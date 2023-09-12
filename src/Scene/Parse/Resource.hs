module Scene.Parse.Resource (interpretResourceTree) where

import Context.App
import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Entity.BaseName (fromTextOptional)
import Entity.Stmt
import Entity.Tree
import Scene.Parse.RawTerm (newAxis, reflRawTerm)

interpretResourceTree :: Tree -> App RawStmt
interpretResourceTree t = do
  ax <- newAxis
  case t of
    m :< Node ts -> do
      case ts of
        [def, name, discarder, copier] -> do
          Throw.liftEither $ chunk "resource" def
          (_, name') <- Throw.liftEither $ getSymbol name >>= fromTextOptional
          discarder' <- Throw.liftEither $ reflRawTerm ax discarder
          copier' <- Throw.liftEither $ reflRawTerm ax copier
          nameLL <- Locator.attachCurrentLocator name'
          return $ RawStmtDefineResource m nameLL discarder' copier'
        _ ->
          Throw.raiseError m "resource"
    m :< _ ->
      Throw.raiseError m "resource"
