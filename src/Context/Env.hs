module Context.Env where

import Context.App
import Context.App.Internal
import Context.Throw qualified as Throw
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Arch qualified as Arch
import Entity.Const
import Entity.DataSize qualified as DS
import Entity.Hint
import Entity.OS qualified as OS
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
  let targetOS = T.pack $ fromMaybe SI.os mTargetOS
  let targetArch = T.pack $ fromMaybe SI.arch mTargetArch
  writeRef targetPlatform $ TargetPlatform {os = OS.reflect targetOS, arch = Arch.reflect targetArch}

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

getDataSize :: Hint -> App DS.DataSize
getDataSize m = do
  tp <- getTargetPlatform
  let mDataSize = Arch.dataSizeOf (arch tp)
  case mDataSize of
    Just dataSize ->
      return dataSize
    Nothing ->
      Throw.raiseError m $ "the data size of the target platform `" <> reify tp <> "` is unknown"
