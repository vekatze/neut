module Scene.Format
  ( format,
    ShouldMinimizeImports,
  )
where

import Context.App
import Context.Env (getMainModule)
import Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Control.Monad
import Data.Text qualified as T
import Entity.Ens.Reify qualified as Ens
import Entity.FileType qualified as FT
import Entity.RawProgram.Decode qualified as RawProgram
import Entity.Target
import Path
import Scene.Ens.Reflect qualified as Ens
import Scene.Initialize qualified as Initialize
import Scene.Load qualified as Load
import Scene.Module.GetEnabledPreset
import Scene.Parse qualified as Parse
import Scene.Parse.Core qualified as P
import Scene.Parse.Program qualified as Parse
import Scene.Unravel qualified as Unravel
import UnliftIO.Async
import Prelude hiding (log)

format :: ShouldMinimizeImports -> FT.FileType -> Path Abs File -> T.Text -> App T.Text
format shouldMinimizeImports fileType path content = do
  case fileType of
    FT.Ens -> do
      Ens.pp <$> Ens.fromFilePath' path content
    FT.Source -> do
      _formatSource shouldMinimizeImports path content

type ShouldMinimizeImports =
  Bool

_formatSource :: ShouldMinimizeImports -> Path Abs File -> T.Text -> App T.Text
_formatSource shouldMinimizeImports path content = do
  Initialize.initializeForTarget
  mainModule <- getMainModule
  if shouldMinimizeImports
    then do
      (_, dependenceSeq) <- Unravel.unravel mainModule $ Main (emptyZen path)
      contentSeq <- pooledForConcurrently dependenceSeq $ \source -> do
        cacheOrContent <- Load.load Peripheral source
        return (source, cacheOrContent)
      let contentSeq' = _replaceLast content contentSeq
      forM_ contentSeq' $ \(source, cacheOrContent) -> do
        Initialize.initializeForSource source
        void $ Parse.parse Peripheral source cacheOrContent
      unusedGlobalLocators <- UnusedGlobalLocator.get
      unusedLocalLocators <- UnusedLocalLocator.get
      program <- P.parseFile True Parse.parseProgram path content
      presetNames <- getEnabledPreset mainModule
      let importInfo = RawProgram.ImportInfo {presetNames, unusedGlobalLocators, unusedLocalLocators}
      return $ RawProgram.pp importInfo program
    else do
      program <- P.parseFile True Parse.parseProgram path content
      presetNames <- getEnabledPreset mainModule
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
