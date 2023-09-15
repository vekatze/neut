module Scene.Parse.Define (interpretDefineTree) where

import Context.App
import Context.Env qualified as Env
import Context.Locator qualified as Locator
import Context.Remark (printNote')
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Atom qualified as AT
import Entity.BaseName (fromTextOptional)
import Entity.Const (macroMaxStep)
import Entity.Hint
import Entity.Macro.Reduce qualified as Macro
import Entity.Opacity qualified as O
import Entity.Stmt
import Entity.StmtKind qualified as SK
import Entity.Tree
import Scene.Parse.RawTerm (newAxis, reflArgList, reflRawTerm)

interpretDefineTree :: Hint -> [Tree] -> App RawStmt
interpretDefineTree m ts = do
  let (ts', attrs) = splitAttrs ts
  rules <- Env.getMacroEnv
  case ts' of
    [] ->
      Throw.raiseError m "unexpected end of form"
    (name : rest) -> do
      ax <- newAxis
      (_, name') <- Throw.liftEither $ getSymbol name >>= fromTextOptional
      (argList, cod, body) <- Throw.liftEither $ reflArrowArgs m rest
      argList' <- Throw.liftEither $ reflArgList ax argList
      cod' <- Throw.liftEither $ reflRawTerm ax cod
      expandedBody <- Throw.liftEither $ Macro.reduce macroMaxStep rules body
      printNote' "body (after)"
      printNote' $ showTree expandedBody
      body' <- Throw.liftEither $ reflRawTerm ax expandedBody
      let clarity = getClarity attrs
      let stmtKind = SK.Normal clarity
      nameLL <- Locator.attachCurrentLocator name'
      let impArgNum = AN.fromInt 0
      return $ RawStmtDefine False stmtKind m nameLL impArgNum argList' cod' body'

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
