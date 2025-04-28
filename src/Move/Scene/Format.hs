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
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.Locator qualified as Locator
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.Tag qualified as Tag
import Move.Context.Unused qualified as Unused
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Ens.Reflect qualified as EnsReflect
import Move.Scene.Init.Source qualified as InitSource
import Move.Scene.Init.Target qualified as InitTarget
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
    envHandle :: Env.Handle,
    ensReflectHandle :: EnsReflect.Handle,
    getEnabledPresetHandle :: GetEnabledPreset.Handle,
    unusedHandle :: Unused.Handle,
    initTargetHandle :: InitTarget.Handle,
    initSourceHandle :: InitSource.Handle
  }

new :: Env.Handle -> Gensym.Handle -> Debug.Handle -> Locator.Handle -> OptimizableData.Handle -> KeyArg.Handle -> Unused.Handle -> Tag.Handle -> Antecedent.Handle -> App Handle
new envHandle gensymHandle debugHandle locatorHandle optDataHandle keyArgHandle unusedHandle tagHandle antecedentHandle = do
  unravelHandle <- Unravel.new envHandle gensymHandle debugHandle locatorHandle optDataHandle keyArgHandle tagHandle antecedentHandle
  loadHandle <- Load.new envHandle debugHandle
  parseCoreHandle <- ParseCore.new gensymHandle
  parseHandle <- Parse.new envHandle gensymHandle debugHandle locatorHandle optDataHandle keyArgHandle tagHandle antecedentHandle
  ensReflectHandle <- EnsReflect.new gensymHandle
  getEnabledPresetHandle <- GetEnabledPreset.new envHandle gensymHandle
  initTargetHandle <- InitTarget.new envHandle gensymHandle debugHandle locatorHandle optDataHandle keyArgHandle tagHandle antecedentHandle
  initSourceHandle <- InitSource.new envHandle locatorHandle optDataHandle keyArgHandle unusedHandle tagHandle antecedentHandle
  return $ Handle {..}

format :: Handle -> ShouldMinimizeImports -> FT.FileType -> Path Abs File -> T.Text -> EIO T.Text
format h shouldMinimizeImports fileType path content = do
  case fileType of
    FT.Ens -> do
      ens <- EnsReflect.fromFilePath' (ensReflectHandle h) path content
      return $ Ens.pp ens
    FT.Source -> do
      _formatSource h shouldMinimizeImports path content

type ShouldMinimizeImports =
  Bool

_formatSource :: Handle -> ShouldMinimizeImports -> Path Abs File -> T.Text -> EIO T.Text
_formatSource h shouldMinimizeImports filePath fileContent = do
  liftIO $ InitTarget.initializeForTarget (initTargetHandle h)
  MainModule mainModule <- Env.getMainModule (envHandle h)
  if shouldMinimizeImports
    then do
      (_, dependenceSeq) <- Unravel.unravel (unravelHandle h) mainModule $ Main (emptyZen filePath)
      contentSeq <- Load.load (loadHandle h) Peripheral dependenceSeq
      let contentSeq' = _replaceLast fileContent contentSeq
      forM_ contentSeq' $ \(source, cacheOrContent) -> do
        InitSource.initializeForSource (initSourceHandle h) source
        void $ Parse.parse (parseHandle h) Peripheral source cacheOrContent
      unusedGlobalLocators <- liftIO $ Unused.getGlobalLocator (unusedHandle h)
      unusedLocalLocators <- liftIO $ Unused.getLocalLocator (unusedHandle h)
      program <- ParseCore.parseFile (parseCoreHandle h) filePath fileContent True Parse.parseProgram
      presetNames <- GetEnabledPreset.getEnabledPreset (getEnabledPresetHandle h) mainModule
      let importInfo = RawProgram.ImportInfo {presetNames, unusedGlobalLocators, unusedLocalLocators}
      return $ RawProgram.pp importInfo program
    else do
      program <- ParseCore.parseFile (parseCoreHandle h) filePath fileContent True Parse.parseProgram
      presetNames <- GetEnabledPreset.getEnabledPreset (getEnabledPresetHandle h) mainModule
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
