module Act.Check
  ( check,
    Config (..),
    Context,
  )
where

import Context.Env qualified as Env
import Context.Log qualified as Log
import Context.Module qualified as Module
import Context.Throw qualified as Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Entity.Config.Check
import Entity.Module
import Entity.Source qualified as Source
import Entity.StrictGlobalLocator qualified as SGL
import Path
import Scene.Elaborate qualified as Elaborate
import Scene.Initialize qualified as Initialize
import Scene.Parse qualified as Parse
import Scene.Unravel qualified as Unravel

class
  ( Throw.Context m,
    Log.Context m,
    Module.Context m,
    Env.Context m,
    Unravel.Context m,
    Parse.Context m,
    Initialize.Context m,
    Elaborate.Context m
  ) =>
  Context m

check :: Context m => Config -> m ()
check cfg = do
  Initialize.initializeCompiler (logCfg cfg) True
  mainModule <- Env.getMainModule
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
  (_, _, _, dependenceSeq) <- Unravel.unravel' initialSource
  forM_ dependenceSeq $ \source -> do
    Initialize.initializeForSource source
    void $ Parse.parse >>= Elaborate.elaborate

ensureFileModuleSanity :: Throw.Context m => Path Abs File -> Module -> m ()
ensureFileModuleSanity filePath mainModule = do
  unless (isProperPrefixOf (getSourceDir mainModule) filePath) $ do
    Throw.raiseError' "the specified file is not in the current module"
