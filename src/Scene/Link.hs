module Scene.Link
  ( Context,
    link,
  )
where

import Context.Env qualified as Env
import Context.LLVM qualified as LLVM
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad.Catch
import Entity.Module
import Entity.OutputKind qualified as OK
import Entity.Source qualified as Source
import Entity.Target
import Scene.Module.Path

class
  ( Throw.Context m,
    Path.Context m,
    MonadThrow m,
    LLVM.Context m,
    Env.Context m
  ) =>
  Context m

link :: Context m => Target -> Bool -> Bool -> [Source.Source] -> m ()
link target shouldSkipLink isObjectAvailable sourceList = do
  mainModule <- Env.getMainModule
  isExecutableAvailable <- getExecutableOutputPath target mainModule >>= Path.doesFileExist
  if shouldSkipLink || (isObjectAvailable && isExecutableAvailable)
    then return ()
    else link' target mainModule sourceList

link' :: Context m => Target -> Module -> [Source.Source] -> m ()
link' target mainModule sourceList = do
  outputPath <- getExecutableOutputPath target mainModule
  objectPathList <- mapM (Source.sourceToOutputPath OK.Object) sourceList
  LLVM.link objectPathList outputPath
