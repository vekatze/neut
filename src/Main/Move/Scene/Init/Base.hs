module Main.Move.Scene.Init.Base
  ( Handle (..),
    new,
    refresh,
  )
where

import Color.Move.CreateHandle qualified as Color
import Color.Rule.Handle qualified as Color
import Logger.Move.CreateHandle qualified as Logger
import Logger.Rule.Handle qualified as Logger
import Main.Move.Context.Antecedent qualified as Antecedent
import Main.Move.Context.Artifact qualified as Artifact
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.Gensym qualified as Gensym
import Main.Move.Context.GlobalRemark qualified as GlobalRemark
import Main.Move.Context.KeyArg qualified as KeyArg
import Main.Move.Context.Module qualified as Module
import Main.Move.Context.OptimizableData qualified as OptimizableData
import Main.Move.Context.Path qualified as Path
import Main.Move.Context.Platform qualified as Platform
import Main.Move.Context.Type qualified as Type
import Main.Move.Scene.Clarify.Handle.CompDef qualified as CompDef
import Main.Move.Scene.Elaborate.Handle.Def qualified as Definition
import Main.Move.Scene.Elaborate.Handle.WeakDef qualified as WeakDef
import Main.Move.Scene.Parse.Handle.NameMap qualified as NameMap
import Main.Rule.Config.Remark qualified as Remark
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
    nameMapHandle :: NameMap.Handle
  }

new :: Remark.Config -> Maybe (Path Abs File) -> IO Handle
new cfg moduleFilePathOrNone = do
  colorHandle <- Color.createHandle (Remark.shouldColorize cfg) (Remark.shouldColorize cfg)
  loggerHandle <- Logger.createHandle colorHandle (Remark.endOfEntry cfg) (Remark.enableDebugMode cfg)
  gensymHandle <- Gensym.new
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
  nameMapHandle <- NameMap.new
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
  nameMapHandle <- NameMap.new
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
        nameMapHandle
      }
