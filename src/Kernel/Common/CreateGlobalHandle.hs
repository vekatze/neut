module Kernel.Common.CreateGlobalHandle
  ( Handle (..),
    new,
  )
where

import Color.CreateHandle qualified as Color
import Color.Handle qualified as Color
import CommandParser.Config.Remark qualified as Remark
import Data.HashMap.Strict qualified as Map
import Data.IORef (IORef, newIORef)
import Gensym.CreateHandle qualified as Gensym
import Gensym.Handle qualified as Gensym
import Kernel.Clarify.Internal.Handle.CompDef qualified as CompDef
import Kernel.Common.Handle.Global.Antecedent qualified as Antecedent
import Kernel.Common.Handle.Global.Artifact qualified as Artifact
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.GlobalRemark qualified as GlobalRemark
import Kernel.Common.Handle.Global.KeyArg qualified as KeyArg
import Kernel.Common.Handle.Global.Module qualified as Module
import Kernel.Common.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Common.Import
import Kernel.Elaborate.Internal.Handle.Def qualified as Definition
import Kernel.Elaborate.Internal.Handle.WeakDef qualified as WeakDef
import Kernel.Parse.Internal.Handle.GlobalNameMap qualified as GlobalNameMap
import Language.Common.ModuleID qualified as MID
import Logger.CreateHandle qualified as Logger
import Logger.Handle qualified as Logger
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
    globalNameMapHandle :: GlobalNameMap.Handle,
    presetCacheRef :: IORef (Map.HashMap MID.ModuleID [ImportItem])
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
  presetCacheRef <- newIORef Map.empty
  return $ Handle {..}
