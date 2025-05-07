module Kernel.Move.Scene.Parse.Discern.Handle
  ( Handle (..),
    new,
    extend,
    extend',
    extendWithoutInsert,
    extendByNominalEnv,
  )
where

import Gensym.Rule.Handle qualified as Gensym
import Kernel.Move.Context.Env qualified as Env
import Kernel.Move.Context.KeyArg qualified as KeyArg
import Kernel.Move.Context.Locator qualified as Locator
import Kernel.Move.Context.OptimizableData qualified as OptimizableData
import Kernel.Move.Context.Platform qualified as Platform
import Kernel.Move.Context.SymLoc qualified as SymLoc
import Kernel.Move.Context.Tag qualified as Tag
import Kernel.Move.Context.TopCandidate qualified as TopCandidate
import Kernel.Move.Scene.Init.Base qualified as Base
import Kernel.Move.Scene.Init.Local qualified as Local
import Kernel.Move.Scene.Parse.Handle.Alias qualified as Alias
import Kernel.Move.Scene.Parse.Handle.NameMap qualified as NameMap
import Kernel.Move.Scene.Parse.Handle.PreDecl qualified as PreDecl
import Kernel.Move.Scene.Parse.Handle.Unused qualified as Unused
import Kernel.Rule.Layer
import Kernel.Rule.NominalEnv
import Kernel.Rule.VarDefKind
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify qualified as Ident
import Logger.Rule.Hint

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    locatorHandle :: Locator.Handle,
    nameMapHandle :: NameMap.Handle,
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
  NameMap.Handle ->
  Handle
new (Base.Handle {..}) (Local.Handle {..}) nameMapHandle = do
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
