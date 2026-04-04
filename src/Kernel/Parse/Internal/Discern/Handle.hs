module Kernel.Parse.Internal.Discern.Handle
  ( Handle (..),
    new,
    extend',
    extendType',
    extendWithoutInsert,
    extendByNominalEnv,
  )
where

import Gensym.Handle qualified as Gensym
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.CreateLocalHandle qualified as Local
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.KeyArg qualified as KeyArg
import Kernel.Common.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.Handle.Local.SymLoc qualified as SymLoc
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.Handle.Local.TopCandidate qualified as TopCandidate
import Kernel.Common.Module qualified as Module
import Kernel.Parse.Internal.Handle.Alias qualified as Alias
import Kernel.Parse.Internal.Handle.NameMap qualified as NameMap
import Kernel.Parse.Internal.Handle.PreDecl qualified as PreDecl
import Kernel.Parse.Internal.Handle.Unused qualified as Unused
import Kernel.Parse.Layer
import Kernel.Parse.NominalEnv
import Kernel.Parse.Stage
import Kernel.Parse.VarDefKind
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Logger.Hint

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
    currentModule :: Module.Module,
    nameEnv :: NominalEnv,
    typeNameEnv :: NominalEnv,
    currentLayer :: Layer,
    currentStage :: Stage
  }

new ::
  Global.Handle ->
  Local.Handle ->
  NameMap.Handle ->
  Module.Module ->
  Handle
new (Global.Handle {..}) (Local.Handle {..}) nameMapHandle currentModule = do
  let nameEnv = empty
  let typeNameEnv = empty
  let currentLayer = 0
  let currentStage = 0
  Handle {..}

extend :: Handle -> Hint -> Ident -> Layer -> Stage -> VarDefKind -> IO Handle
extend h m newVar l s k = do
  Unused.insertVariable (unusedHandle h) m newVar k
  return $ h {nameEnv = (Ident.toText newVar, (m, newVar, l, s)) : nameEnv h}

extend' :: Handle -> Hint -> Ident -> VarDefKind -> IO Handle
extend' h m newVar k = do
  extend h m newVar (currentLayer h) (currentStage h) k

extendType' :: Handle -> Hint -> Ident -> VarDefKind -> IO Handle
extendType' h m newVar k = do
  Unused.insertVariable (unusedHandle h) m newVar k
  return $ h {typeNameEnv = (Ident.toText newVar, (m, newVar, currentLayer h, currentStage h)) : typeNameEnv h}

extendWithoutInsert :: Handle -> Hint -> Ident -> Handle
extendWithoutInsert h m newVar = do
  h {nameEnv = (Ident.toText newVar, (m, newVar, currentLayer h, currentStage h)) : nameEnv h}

extendByNominalEnv :: Handle -> VarDefKind -> NominalEnv -> IO Handle
extendByNominalEnv h k newNominalEnv = do
  case newNominalEnv of
    [] ->
      return h
    (_, (m, x, l, s)) : rest -> do
      h' <- extend h m x l s k
      extendByNominalEnv h' k rest
