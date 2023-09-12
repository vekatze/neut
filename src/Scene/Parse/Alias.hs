module Scene.Parse.Alias (interpretAliasTree) where

import Context.App
import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Atom qualified as AT
import Entity.BaseName (fromTextOptional)
import Entity.Opacity qualified as O
import Entity.RawTerm qualified as RT
import Entity.Stmt
import Entity.StmtKind qualified as SK
import Entity.Tree
import Scene.Parse.RawTerm (newAxis, reflRawTerm)

interpretAliasTree :: Tree -> App RawStmt
interpretAliasTree t = do
  ax <- newAxis
  case t of
    m :< Node ts -> do
      let (ts', attrs) = splitAttrs ts
      case ts' of
        [def, name, body] -> do
          Throw.liftEither $ chunk "alias" def
          (_, name') <- Throw.liftEither $ getSymbol name >>= fromTextOptional
          body' <- Throw.liftEither $ reflRawTerm ax body
          let clarity = getClarity attrs
          let stmtKind = SK.Normal clarity
          nameLL <- Locator.attachCurrentLocator name'
          let impArgNum = AN.fromInt 0
          return $ RawStmtDefine True stmtKind m nameLL impArgNum [] (m :< RT.Tau) body'
        _ ->
          Throw.raiseError m "alias"
    m :< _ ->
      Throw.raiseError m "alias"

getClarity :: Map.HashMap T.Text Tree -> O.Opacity
getClarity attrs = do
  case Map.lookup "clarity" attrs of
    Just (_ :< Atom (AT.Symbol symbol))
      | symbol == "high" ->
          O.Transparent
      | symbol == "low" ->
          O.Opaque
    _ ->
      O.Opaque
