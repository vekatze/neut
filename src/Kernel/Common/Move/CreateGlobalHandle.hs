module Kernel.Common.Move.CreateGlobalHandle
  ( Handle (..),
    new,
  )
where

import Color.Move.CreateHandle qualified as Color
import Color.Rule.Handle qualified as Color
import CommandParser.Rule.Config.Remark qualified as Remark
import Gensym.Move.CreateHandle qualified as Gensym
import Gensym.Rule.Handle qualified as Gensym
import Logger.Move.CreateHandle qualified as Logger
import Logger.Rule.Handle qualified as Logger
import Kernel.Clarify.Move.Internal.Handle.CompDef qualified as CompDef
import Kernel.Common.Move.Handle.Global.Antecedent qualified as Antecedent
import Kernel.Common.Move.Handle.Global.Artifact qualified as Artifact
import Kernel.Common.Move.Handle.Global.Env qualified as Env
import Kernel.Common.Move.Handle.Global.GlobalRemark qualified as GlobalRemark
import Kernel.Common.Move.Handle.Global.KeyArg qualified as KeyArg
import Kernel.Common.Move.Handle.Global.Module qualified as Module
import Kernel.Common.Move.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Move.Handle.Global.Path qualified as Path
import Kernel.Common.Move.Handle.Global.Platform qualified as Platform
import Kernel.Common.Move.Handle.Global.Type qualified as Type
import Kernel.Common.Rule.Handle.Global.Antecedent qualified as Antecedent
import Kernel.Common.Rule.Handle.Global.Artifact qualified as Artifact
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Kernel.Common.Rule.Handle.Global.GlobalRemark qualified as GlobalRemark
import Kernel.Common.Rule.Handle.Global.KeyArg qualified as KeyArg
import Kernel.Common.Rule.Handle.Global.Module qualified as Module
import Kernel.Common.Rule.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Rule.Handle.Global.Path qualified as Path
import Kernel.Common.Rule.Handle.Global.Platform qualified as Platform
import Kernel.Common.Rule.Handle.Global.Type qualified as Type
import Kernel.Elaborate.Move.Internal.Handle.Def qualified as Definition
import Kernel.Elaborate.Move.Internal.Handle.WeakDef qualified as WeakDef
import Kernel.Parse.Move.Internal.Handle.GlobalNameMap qualified as GlobalNameMap
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
  loggerHandle <- Logger.createHandle colorHandle (Remark.enableDebugMode cfg)
  gensymHandle <- Gensym.createHandle
  platformHandle <- Platform.new loggerHandle
  envHandle <- Env.new loggerHandle (Remark.enableSilentMode cfg) moduleFilePathOrNone
  let mainModule = Env.getMainModule envHandle
  keyArgHandle <- KeyArg.new mainModule
  optDataHandle <- OptimizableData.new
  typeHandle <- Type.new
  pathHandle <- Path.new mainModule platformHandle loggerHandle
  globalRemarkHandle <- GlobalRemark.new
  artifactHandle <- Artifact.new
  moduleHandle <- Module.new
  weakDefHandle <- WeakDef.new gensymHandle
  defHandle <- Definition.new
  antecedentHandle <- Antecedent.new
  compDefHandle <- CompDef.new
  globalNameMapHandle <- GlobalNameMap.new
  return $ Handle {..}
