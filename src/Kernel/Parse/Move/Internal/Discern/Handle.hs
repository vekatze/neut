module Kernel.Parse.Move.Internal.Discern.Handle
  ( Handle (..),
    new,
    extend',
    extendWithoutInsert,
    extendByNominalEnv,
  )
where

import Gensym.Rule.Handle qualified as Gensym
import Kernel.Move.Context.Global.Env qualified as Env
import Kernel.Move.Context.Global.KeyArg qualified as KeyArg
import Kernel.Move.Context.Global.OptimizableData qualified as OptimizableData
import Kernel.Move.Context.Global.Platform qualified as Platform
import Kernel.Move.Context.Local.Locator qualified as Locator
import Kernel.Move.Context.Local.SymLoc qualified as SymLoc
import Kernel.Move.Context.Local.Tag qualified as Tag
import Kernel.Move.Context.Local.TopCandidate qualified as TopCandidate
import Kernel.Move.Scene.Init.Global qualified as Global
import Kernel.Move.Scene.Init.Local qualified as Local
import Kernel.Parse.Move.Internal.Handle.Alias qualified as Alias
import Kernel.Parse.Move.Internal.Handle.NameMap qualified as NameMap
import Kernel.Parse.Move.Internal.Handle.PreDecl qualified as PreDecl
import Kernel.Parse.Move.Internal.Handle.Unused qualified as Unused
import Kernel.Parse.Rule.Layer
import Kernel.Parse.Rule.NominalEnv
import Kernel.Parse.Rule.VarDefKind
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
  Global.Handle ->
  Local.Handle ->
  NameMap.Handle ->
  Handle
new (Global.Handle {..}) (Local.Handle {..}) nameMapHandle = do
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
