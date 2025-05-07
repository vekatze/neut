module Kernel.Move.Scene.Init.Base
  ( Handle (..),
    new,
    refresh,
  )
where

import Color.Move.CreateHandle qualified as Color
import Color.Rule.Handle qualified as Color
import CommandParser.Rule.Config.Remark qualified as Remark
import Gensym.Move.CreateHandle qualified as Gensym
import Gensym.Rule.Handle qualified as Gensym
import Kernel.Clarify.Move.Internal.Handle.CompDef qualified as CompDef
import Kernel.Elaborate.Move.Internal.Handle.Def qualified as Definition
import Kernel.Elaborate.Move.Internal.Handle.WeakDef qualified as WeakDef
import Kernel.Move.Context.Antecedent qualified as Antecedent
import Kernel.Move.Context.Artifact qualified as Artifact
import Kernel.Move.Context.Env qualified as Env
import Kernel.Move.Context.GlobalRemark qualified as GlobalRemark
import Kernel.Move.Context.KeyArg qualified as KeyArg
import Kernel.Move.Context.Module qualified as Module
import Kernel.Move.Context.OptimizableData qualified as OptimizableData
import Kernel.Move.Context.Path qualified as Path
import Kernel.Move.Context.Platform qualified as Platform
import Kernel.Move.Context.Type qualified as Type
import Kernel.Parse.Move.Internal.Handle.GlobalNameMap qualified as GlobalNameMap
import Logger.Move.CreateHandle qualified as Logger
import Logger.Rule.Handle qualified as Logger
import Path

data Handle = Handle
  { artifactHandle :: Artifact.Handle,
    antecedentHandle :: Antecedent.Handle,
    colorHandle :: Color.Handle,
    platformHandle :: Platform.Handle,
    defHandle :: Definition.Handle,
    envHandle :: Env.Handle,
    gensymHandle :: Gensym.Handle,
    globalRemarkHandle :: GlobalRemark.Handle,
    keyArgHandle :: KeyArg.Handle,
    moduleHandle :: Module.Handle,
    optDataHandle :: OptimizableData.Handle,
    pathHandle :: Path.Handle,
    loggerHandle :: Logger.Handle,
    typeHandle :: Type.Handle,
    weakDefHandle :: WeakDef.Handle,
    compDefHandle :: CompDef.Handle,
    globalNameMapHandle :: GlobalNameMap.Handle
  }

new :: Remark.Config -> Maybe (Path Abs File) -> IO Handle
new cfg moduleFilePathOrNone = do
  colorHandle <- Color.createHandle (Remark.shouldColorize cfg) (Remark.shouldColorize cfg)
  loggerHandle <- Logger.createHandle colorHandle (Remark.endOfEntry cfg) (Remark.enableDebugMode cfg)
  gensymHandle <- Gensym.createHandle
  platformHandle <- Platform.new loggerHandle
  envHandle <- Env.new loggerHandle (Remark.enableSilentMode cfg) moduleFilePathOrNone
  keyArgHandle <- KeyArg.new envHandle
  optDataHandle <- OptimizableData.new
  typeHandle <- Type.new
  pathHandle <- Path.new envHandle platformHandle loggerHandle
  globalRemarkHandle <- GlobalRemark.new
  artifactHandle <- Artifact.new
  moduleHandle <- Module.new
  weakDefHandle <- WeakDef.new gensymHandle
  defHandle <- Definition.new
  antecedentHandle <- Antecedent.new
  compDefHandle <- CompDef.new
  globalNameMapHandle <- GlobalNameMap.new
  return $ Handle {..}

refresh :: Handle -> IO Handle
refresh h = do
  keyArgHandle <- KeyArg.new (envHandle h)
  optDataHandle <- OptimizableData.new
  typeHandle <- Type.new
  pathHandle <- Path.new (envHandle h) (platformHandle h) (loggerHandle h)
  globalRemarkHandle <- GlobalRemark.new
  artifactHandle <- Artifact.new
  moduleHandle <- Module.new
  weakDefHandle <- WeakDef.new (gensymHandle h)
  defHandle <- Definition.new
  antecedentHandle <- Antecedent.new
  compDefHandle <- CompDef.new
  globalNameMapHandle <- GlobalNameMap.new
  return $
    h
      { keyArgHandle,
        optDataHandle,
        typeHandle,
        pathHandle,
        globalRemarkHandle,
        artifactHandle,
        moduleHandle,
        weakDefHandle,
        defHandle,
        antecedentHandle,
        compDefHandle,
        globalNameMapHandle
      }
