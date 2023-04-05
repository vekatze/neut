module Context.Env where

import Context.App
import Context.App.Internal
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Entity.AliasInfo
import Entity.Const
import Entity.Source qualified as Source
import Entity.TargetPlatform
import Path
import System.Environment
import System.Info qualified as SI

getTargetPlatform :: App TargetPlatform
getTargetPlatform =
  readRef "targetPlatform" targetPlatform

setTargetPlatform :: App ()
setTargetPlatform = do
  mTargetArch <- liftIO $ lookupEnv envVarTargetArch
  mTargetOS <- liftIO $ lookupEnv envVarTargetOS
  let targetOS = fromMaybe SI.os mTargetOS
  let targetArch = fromMaybe SI.arch mTargetArch
  writeRef targetPlatform $ TargetPlatform {os = targetOS, arch = targetArch}

setCurrentSource :: Source.Source -> App ()
setCurrentSource =
  writeRef currentSource

getCurrentSource :: App Source.Source
getCurrentSource =
  readRef "currentSource" currentSource

type PathSet = S.Set (Path Abs File)

getHasCacheSet :: App PathSet
getHasCacheSet =
  readRef' hasCacheSet

getHasObjectSet :: App PathSet
getHasObjectSet =
  readRef' hasObjectSet

getHasLLVMSet :: App PathSet
getHasLLVMSet =
  readRef' hasLLVMSet

insertToHasObjectSet :: Path Abs File -> App ()
insertToHasObjectSet v =
  modifyRef' hasObjectSet $ S.insert v

insertToHasCacheSet :: Path Abs File -> App ()
insertToHasCacheSet v =
  modifyRef' hasCacheSet $ S.insert v

insertToHasLLVMSet :: Path Abs File -> App ()
insertToHasLLVMSet v =
  modifyRef' hasLLVMSet $ S.insert v

getSourceAliasMap :: App SourceAliasMap
getSourceAliasMap =
  readRef' sourceAliasMap

insertToSourceAliasMap :: Path Abs File -> [AliasInfo] -> App ()
insertToSourceAliasMap k v =
  modifyRef' sourceAliasMap $ Map.insert k v
