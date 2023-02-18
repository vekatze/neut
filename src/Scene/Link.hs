module Scene.Link
  ( link,
  )
where

import Context.App
import Context.LLVM qualified as LLVM
import Context.Module qualified as Module
import Context.Path qualified as Path
import Entity.Module
import Entity.OutputKind qualified as OK
import Entity.Source qualified as Source
import Entity.Target

link :: Target -> Bool -> Bool -> [Source.Source] -> App ()
link target shouldSkipLink isObjectAvailable sourceList = do
  mainModule <- Module.getMainModule
  isExecutableAvailable <- Path.getExecutableOutputPath target mainModule >>= Path.doesFileExist
  if shouldSkipLink || (isObjectAvailable && isExecutableAvailable)
    then return ()
    else link' target mainModule sourceList

link' :: Target -> Module -> [Source.Source] -> App ()
link' target mainModule sourceList = do
  outputPath <- Path.getExecutableOutputPath target mainModule
  objectPathList <- mapM (Source.sourceToOutputPath OK.Object) sourceList
  LLVM.link objectPathList outputPath
