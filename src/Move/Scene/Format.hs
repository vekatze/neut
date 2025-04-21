module Move.Scene.Format
  ( format,
    ShouldMinimizeImports,
  )
where

import Move.Context.App
import Move.Context.Env (getMainModule)
import Move.Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Move.Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Control.Monad
import Data.Text qualified as T
import Rule.Ens.Reify qualified as Ens
import Rule.FileType qualified as FT
import Rule.RawProgram.Decode qualified as RawProgram
import Rule.Target
import Path
import Move.Scene.Ens.Reflect qualified as Ens
import Move.Scene.Initialize qualified as Initialize
import Move.Scene.Load qualified as Load
import Move.Scene.Module.GetEnabledPreset
import Move.Scene.Parse qualified as Parse
import Move.Scene.Parse.Core qualified as P
import Move.Scene.Parse.Program qualified as Parse
import Move.Scene.Unravel qualified as Unravel
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
      contentSeq <- Load.load Peripheral dependenceSeq
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
