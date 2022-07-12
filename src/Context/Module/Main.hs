module Context.Module.Main (new) where

import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import qualified Data.Text as T
import qualified Entity.Hint as H
import Entity.Module
import qualified Entity.ModuleChecksum as MC
import qualified Entity.ModuleID as MID
import Path
import Path.IO

new :: Module.Config -> IO Module.Context
new cfg =
  return $
    Module.Context
      { Module.getModuleFilePath =
          getModuleFilePath (Module.throwCtx cfg) (Module.pathCtx cfg) (Module.mainModule cfg)
      }

getModuleFilePath :: Throw.Context -> Path.Context -> Module -> Maybe H.Hint -> MID.ModuleID -> IO (Path Abs File)
getModuleFilePath throwCtx pathCtx mainModule mHint moduleID = do
  case moduleID of
    MID.Base -> do
      let message = "the base module can't be used here"
      case mHint of
        Just hint ->
          Throw.raiseError throwCtx hint message
        Nothing ->
          Throw.raiseError' throwCtx message
    MID.This ->
      return $ moduleLocation mainModule
    MID.That (MC.ModuleChecksum checksum) -> do
      libraryDir <- Path.getLibraryDirPath pathCtx
      moduleDir <- resolveDir libraryDir $ T.unpack checksum
      return $ moduleDir </> moduleFile
