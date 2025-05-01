module Move.Scene.Init.Base
  ( Handle (..),
    new,
  )
where

import Move.Console.Report qualified as Report
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.Artifact qualified as Artifact
import Move.Context.Clang qualified as Clang
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
import Move.Context.Type qualified as Type
import Move.Context.WeakDefinition qualified as WeakDefinition
import Move.Language.Utility.Gensym qualified as Gensym
import Move.UI.Handle.GlobalRemark qualified as GlobalRemark

data Handle
  = Handle
  { artifactHandle :: Artifact.Handle,
    antecedentHandle :: Antecedent.Handle,
    clangHandle :: Clang.Handle,
    colorHandle :: Color.Handle,
    debugHandle :: Debug.Handle,
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

new :: IO Handle
new = do
  colorHandle <- Color.new
  reportHandle <- Report.new colorHandle
  gensymHandle <- Gensym.new
  envHandle <- Env.new reportHandle
  debugHandle <- Debug.new colorHandle
  keyArgHandle <- KeyArg.new envHandle
  optDataHandle <- OptimizableData.new
  typeHandle <- Type.new
  clangHandle <- Clang.new debugHandle
  pathHandle <- Path.new envHandle debugHandle clangHandle
  globalRemarkHandle <- GlobalRemark.new
  artifactHandle <- Artifact.new
  moduleHandle <- Module.new
  weakDefHandle <- WeakDefinition.new gensymHandle
  defHandle <- Definition.new
  antecedentHandle <- Antecedent.new
  compDefHandle <- CompDefinition.new
  nameMapHandle <- NameMap.new
  return $ Handle {..}
