module Main.Move.Scene.Parse.Discern.Handle
  ( Handle (..),
    new,
    extend,
    extend',
    extendWithoutInsert,
    extendByNominalEnv,
  )
where

import Gensym.Rule.Handle qualified as Gensym
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify qualified as Ident
import Logger.Rule.Hint
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.KeyArg qualified as KeyArg
import Main.Move.Context.Locator qualified as Locator
import Main.Move.Context.OptimizableData qualified as OptimizableData
import Main.Move.Context.Platform qualified as Platform
import Main.Move.Context.SymLoc qualified as SymLoc
import Main.Move.Context.Tag qualified as Tag
import Main.Move.Context.TopCandidate qualified as TopCandidate
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Init.Local qualified as Local
import Main.Move.Scene.Parse.Handle.Alias qualified as Alias
import Main.Move.Scene.Parse.Handle.Global qualified as Global
import Main.Move.Scene.Parse.Handle.PreDecl qualified as PreDecl
import Main.Move.Scene.Parse.Handle.Unused qualified as Unused
import Main.Rule.Layer
import Main.Rule.NominalEnv
import Main.Rule.VarDefKind

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    locatorHandle :: Locator.Handle,
    globalHandle :: Global.Handle,
    aliasHandle :: Alias.Handle,
    tagHandle :: Tag.Handle,
    keyArgHandle :: KeyArg.Handle,
    symLocHandle :: SymLoc.Handle,
    topCandidateHandle :: TopCandidate.Handle,
    preDeclHandle :: PreDecl.Handle,
    optDataHandle :: OptimizableData.Handle,
    unusedHandle :: Unused.Handle,
    envHandle :: Env.Handle,
    platformHandle :: Platform.Handle,
    nameEnv :: NominalEnv,
    currentLayer :: Layer
  }

new ::
  Base.Handle ->
  Local.Handle ->
  Global.Handle ->
  Handle
new (Base.Handle {..}) (Local.Handle {..}) globalHandle = do
  let nameEnv = empty
  let currentLayer = 0
  Handle {..}

extend :: Handle -> Hint -> Ident -> Layer -> VarDefKind -> IO Handle
extend h m newVar l k = do
  Unused.insertVariable (unusedHandle h) m newVar k
  return $ h {nameEnv = (Ident.toText newVar, (m, newVar, l)) : nameEnv h}

extend' :: Handle -> Hint -> Ident -> VarDefKind -> IO Handle
extend' h m newVar k = do
  extend h m newVar (currentLayer h) k

extendWithoutInsert :: Handle -> Hint -> Ident -> Handle
extendWithoutInsert h m newVar = do
  h {nameEnv = (Ident.toText newVar, (m, newVar, currentLayer h)) : nameEnv h}

extendByNominalEnv :: Handle -> VarDefKind -> NominalEnv -> IO Handle
extendByNominalEnv h k newNominalEnv = do
  case newNominalEnv of
    [] ->
      return h
    (_, (m, x, l)) : rest -> do
      h' <- extend h m x l k
      extendByNominalEnv h' k rest
