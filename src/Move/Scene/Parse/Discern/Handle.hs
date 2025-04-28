module Move.Scene.Parse.Discern.Handle
  ( Handle (..),
    new,
    extend,
    extend',
    extendWithoutInsert,
    extendByNominalEnv,
  )
where

import Move.Context.Alias qualified as Alias
import Move.Context.App
import Move.Context.Env qualified as Env
import Move.Context.Global qualified as Global
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.Locator qualified as Locator
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.PreDecl qualified as PreDecl
import Move.Context.SymLoc qualified as SymLoc
import Move.Context.Tag qualified as Tag
import Move.Context.TopCandidate qualified as TopCandidate
import Move.Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Move.Context.UnusedStaticFile qualified as UnusedStaticFile
import Move.Context.UnusedVariable qualified as UnusedVariable
import Move.Language.Utility.Gensym qualified as Gensym
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.Layer
import Rule.NominalEnv
import Rule.VarDefKind

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
    unusedVariableHandle :: UnusedVariable.Handle,
    unusedLocalLocatorHandle :: UnusedLocalLocator.Handle,
    unusedStaticFileHandle :: UnusedStaticFile.Handle,
    envHandle :: Env.Handle,
    nameEnv :: NominalEnv,
    currentLayer :: Layer
  }

new :: Env.Handle -> Gensym.Handle -> App Handle
new envHandle gensymHandle = do
  locatorHandle <- Locator.new
  globalHandle <- Global.new envHandle
  aliasHandle <- Alias.new envHandle
  keyArgHandle <- KeyArg.new envHandle
  symLocHandle <- SymLoc.new
  topCandidateHandle <- TopCandidate.new
  tagHandle <- Tag.new
  preDeclHandle <- PreDecl.new
  optDataHandle <- OptimizableData.new
  unusedStaticFileHandle <- UnusedStaticFile.new
  unusedVariableHandle <- UnusedVariable.new
  unusedLocalLocatorHandle <- UnusedLocalLocator.new
  let nameEnv = empty
  let currentLayer = 0
  return $ Handle {..}

extend :: Handle -> Hint -> Ident -> Layer -> VarDefKind -> IO Handle
extend h m newVar l k = do
  UnusedVariable.insert (unusedVariableHandle h) m newVar k
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
