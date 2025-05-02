module Move.Scene.Init.Base
  ( Handle (..),
    new,
    refresh,
  )
where

import Move.Console.Report qualified as Report
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.Artifact qualified as Artifact
import Move.Context.Color qualified as Color
import Move.Context.CompDefinition qualified as CompDefinition
import Move.Context.Debug qualified as Debug
import Move.Context.Definition qualified as Definition
import Move.Context.Env qualified as Env
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.Module qualified as Module
import Move.Context.NameMap qualified as NameMap
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.Path qualified as Path
import Move.Context.Platform qualified as Platform
import Move.Context.Type qualified as Type
import Move.Context.WeakDefinition qualified as WeakDefinition
import Move.Language.Utility.Gensym qualified as Gensym
import Move.UI.Handle.GlobalRemark qualified as GlobalRemark
import Path
import Rule.Config.Remark qualified as Remark

data Handle
  = Handle
  { artifactHandle :: Artifact.Handle,
    antecedentHandle :: Antecedent.Handle,
    colorHandle :: Color.Handle,
    debugHandle :: Debug.Handle,
    platformHandle :: Platform.Handle,
    defHandle :: Definition.Handle,
    envHandle :: Env.Handle,
    gensymHandle :: Gensym.Handle,
    globalRemarkHandle :: GlobalRemark.Handle,
    keyArgHandle :: KeyArg.Handle,
    moduleHandle :: Module.Handle,
    optDataHandle :: OptimizableData.Handle,
    pathHandle :: Path.Handle,
    reportHandle :: Report.Handle,
    typeHandle :: Type.Handle,
    weakDefHandle :: WeakDefinition.Handle,
    compDefHandle :: CompDefinition.Handle,
    nameMapHandle :: NameMap.Handle
  }

new :: Remark.Config -> Maybe (Path Abs File) -> IO Handle
new cfg moduleFilePathOrNone = do
  colorHandle <- Color.new (Remark.shouldColorize cfg)
  let reportHandle = Report.new colorHandle (Remark.endOfEntry cfg)
  gensymHandle <- Gensym.new
  debugHandle <- Debug.new colorHandle (Remark.enableDebugMode cfg)
  platformHandle <- Platform.new reportHandle debugHandle
  envHandle <- Env.new reportHandle (Remark.enableSilentMode cfg) moduleFilePathOrNone
  keyArgHandle <- KeyArg.new envHandle
  optDataHandle <- OptimizableData.new
  typeHandle <- Type.new
  pathHandle <- Path.new envHandle platformHandle debugHandle
  globalRemarkHandle <- GlobalRemark.new
  artifactHandle <- Artifact.new
  moduleHandle <- Module.new
  weakDefHandle <- WeakDefinition.new gensymHandle
  defHandle <- Definition.new
  antecedentHandle <- Antecedent.new
  compDefHandle <- CompDefinition.new
  nameMapHandle <- NameMap.new
  return $ Handle {..}

refresh :: Handle -> IO Handle
refresh h = do
  keyArgHandle <- KeyArg.new (envHandle h)
  optDataHandle <- OptimizableData.new
  typeHandle <- Type.new
  pathHandle <- Path.new (envHandle h) (platformHandle h) (debugHandle h)
  globalRemarkHandle <- GlobalRemark.new
  artifactHandle <- Artifact.new
  moduleHandle <- Module.new
  weakDefHandle <- WeakDefinition.new (gensymHandle h)
  defHandle <- Definition.new
  antecedentHandle <- Antecedent.new
  compDefHandle <- CompDefinition.new
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
