module Act.Check
  ( check,
    Config (..),
    Context,
  )
where

import qualified Context.Env as Env
import qualified Context.Log as Log
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.HashMap.Strict as Map
import Entity.Module
import qualified Entity.Module.Reflect as Module
import qualified Entity.Source as Source
import qualified Entity.StrictGlobalLocator as SGL
import Path
import qualified Scene.Elaborate as Elaborate
import qualified Scene.Parse as Parse
import qualified Scene.Unravel as Unravel

data Config = Config
  { mFilePathString :: Maybe FilePath,
    logCfg :: Log.Config
  }

class
  ( Throw.Context m,
    Log.Context m,
    Path.Context m,
    Module.Context m,
    Env.Context m,
    Unravel.Context m,
    Parse.Context m,
    Elaborate.Context m
  ) =>
  Context m

check :: Context m => Config -> m ()
check cfg = do
  Env.setEndOfEntry $ Log.endOfEntry $ logCfg cfg
  Env.setShouldColorize $ Log.shouldColorize $ logCfg cfg
  Throw.run $ do
    Path.ensureNotInLibDir
    mainModule <- Module.fromCurrentPath
    Env.setMainModule mainModule
    Env.setTargetPlatform
    case mFilePathString cfg of
      Just filePathStr -> do
        sgl <- SGL.reflectInMainModule filePathStr
        check' sgl mainModule
      Nothing -> do
        forM_ (Map.elems $ moduleTarget mainModule) $ \sgl ->
          check' sgl mainModule

check' ::
  Context m =>
  SGL.StrictGlobalLocator ->
  Module ->
  m ()
check' sgl mainModule = do
  filePath <- Module.getSourcePath sgl
  ensureFileModuleSanity filePath mainModule
  let initialSource = Source.Source {Source.sourceModule = mainModule, Source.sourceFilePath = filePath}
  (_, _, _, dependenceSeq) <- Unravel.unravel initialSource
  forM_ dependenceSeq $ \source -> do
    void $ Parse.parse source >>= Elaborate.elaborate source

ensureFileModuleSanity :: Throw.Context m => Path Abs File -> Module -> m ()
ensureFileModuleSanity filePath mainModule = do
  unless (isProperPrefixOf (getSourceDir mainModule) filePath) $ do
    Throw.raiseError' "the specified file is not in the current module"
