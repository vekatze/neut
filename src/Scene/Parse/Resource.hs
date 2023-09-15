module Scene.Parse.Resource (interpretResourceTree) where

import Context.App
import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Entity.BaseName (fromTextOptional)
import Entity.Hint
import Entity.Stmt
import Entity.Tree
import Scene.Parse.RawTerm (newAxis, reflRawTerm)

interpretResourceTree :: Hint -> [Tree] -> App RawStmt
interpretResourceTree m ts = do
  ax <- newAxis
  case ts of
    [name, discarder, copier] -> do
      (_, name') <- Throw.liftEither $ getSymbol name >>= fromTextOptional
      discarder' <- Throw.liftEither $ reflRawTerm ax discarder
      copier' <- Throw.liftEither $ reflRawTerm ax copier
      nameLL <- Locator.attachCurrentLocator name'
      return $ RawStmtDefineResource m nameLL discarder' copier'
    _ ->
      Throw.raiseError m "resource"
