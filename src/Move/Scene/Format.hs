module Move.Scene.Format
  ( Handle,
    new,
    format,
    ShouldMinimizeImports,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env (getMainModule)
import Move.Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Move.Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Move.Scene.Ens.Reflect qualified as EnsReflect
import Move.Scene.Initialize qualified as Initialize
import Move.Scene.Load qualified as Load
import Move.Scene.Module.GetEnabledPreset qualified as GetEnabledPreset
import Move.Scene.Parse qualified as Parse
import Move.Scene.Parse.Core qualified as ParseCore
import Move.Scene.Parse.Program qualified as Parse
import Move.Scene.Unravel qualified as Unravel
import Path
import Rule.Ens.Reify qualified as Ens
import Rule.FileType qualified as FT
import Rule.Module (MainModule (MainModule))
import Rule.RawProgram.Decode qualified as RawProgram
import Rule.Target
import Prelude hiding (log)

data Handle = Handle
  { unravelHandle :: Unravel.Handle,
    loadHandle :: Load.Handle,
    parseCoreHandle :: ParseCore.Handle,
    parseHandle :: Parse.Handle,
    ensReflectHandle :: EnsReflect.Handle,
    getEnabledPresetHandle :: GetEnabledPreset.Handle,
    unusedGlobalLocatorHandle :: UnusedGlobalLocator.Handle
  }

new :: App Handle
new = do
  unravelHandle <- Unravel.new
  loadHandle <- Load.new
  parseCoreHandle <- ParseCore.new
  parseHandle <- Parse.new
  ensReflectHandle <- EnsReflect.new
  getEnabledPresetHandle <- GetEnabledPreset.new
  unusedGlobalLocatorHandle <- UnusedGlobalLocator.new
  return $ Handle {..}

format :: ShouldMinimizeImports -> FT.FileType -> Path Abs File -> T.Text -> App T.Text
format shouldMinimizeImports fileType path content = do
  h <- new
  case fileType of
    FT.Ens -> do
      ens <- toApp $ EnsReflect.fromFilePath' (ensReflectHandle h) path content
      return $ Ens.pp ens
    FT.Source -> do
      _formatSource h shouldMinimizeImports path content

type ShouldMinimizeImports =
  Bool

_formatSource :: Handle -> ShouldMinimizeImports -> Path Abs File -> T.Text -> App T.Text
_formatSource h shouldMinimizeImports filePath fileContent = do
  Initialize.initializeForTarget
  MainModule mainModule <- getMainModule
  if shouldMinimizeImports
    then do
      (_, dependenceSeq) <- toApp $ Unravel.unravel (unravelHandle h) mainModule $ Main (emptyZen filePath)
      contentSeq <- toApp $ Load.load (loadHandle h) Peripheral dependenceSeq
      let contentSeq' = _replaceLast fileContent contentSeq
      forM_ contentSeq' $ \(source, cacheOrContent) -> do
        Initialize.initializeForSource source
        void $ toApp $ Parse.parse (parseHandle h) Peripheral source cacheOrContent
      unusedGlobalLocators <- liftIO $ UnusedGlobalLocator.get (unusedGlobalLocatorHandle h)
      unusedLocalLocators <- UnusedLocalLocator.get
      program <- toApp $ ParseCore.parseFile (parseCoreHandle h) filePath fileContent True Parse.parseProgram
      presetNames <- toApp $ GetEnabledPreset.getEnabledPreset (getEnabledPresetHandle h) mainModule
      let importInfo = RawProgram.ImportInfo {presetNames, unusedGlobalLocators, unusedLocalLocators}
      return $ RawProgram.pp importInfo program
    else do
      program <- toApp $ ParseCore.parseFile (parseCoreHandle h) filePath fileContent True Parse.parseProgram
      presetNames <- toApp $ GetEnabledPreset.getEnabledPreset (getEnabledPresetHandle h) mainModule
      let importInfo = RawProgram.ImportInfo {presetNames, unusedGlobalLocators = [], unusedLocalLocators = []}
      return $ RawProgram.pp importInfo program

_replaceLast :: T.Text -> [(a, Either b T.Text)] -> [(a, Either b T.Text)]
_replaceLast content contentSeq =
  case contentSeq of
    [] ->
      []
    [(source, _)] ->
      [(source, Right content)]
    cacheOrContent : rest ->
      cacheOrContent : _replaceLast content rest
