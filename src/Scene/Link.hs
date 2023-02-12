module Scene.Link
  ( Context,
    link,
  )
where

import Context.Env qualified as Env
import Context.LLVM qualified as LLVM
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Entity.Module
import Entity.OutputKind qualified as OK
import Entity.Source qualified as Source
import Entity.Target

class
  ( Throw.Context m,
    Path.Context m,
    Source.Context m,
    LLVM.Context m,
    Env.Context m
  ) =>
  Context m

link :: Context m => Target -> String -> Bool -> Bool -> [Source.Source] -> m ()
link target clangOptString shouldSkipLink isObjectAvailable sourceList = do
  mainModule <- Env.getMainModule
  isExecutableAvailable <- getExecutableOutputPath target mainModule >>= Path.doesFileExist
  if shouldSkipLink || (isObjectAvailable && isExecutableAvailable)
    then return ()
    else link' clangOptString target mainModule sourceList

link' :: Context m => String -> Target -> Module -> [Source.Source] -> m ()
link' clangOptString target mainModule sourceList = do
  outputPath <- getExecutableOutputPath target mainModule
  objectPathList <- mapM (Source.sourceToOutputPath OK.Object) sourceList
  LLVM.link clangOptString objectPathList outputPath
