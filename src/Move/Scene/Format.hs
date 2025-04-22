module Move.Scene.Format
  ( format,
    ShouldMinimizeImports,
  )
where

import Control.Monad
import Control.Monad.Reader (asks)
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (toApp)
import Move.Context.Env (getMainModule)
import Move.Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Move.Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Move.Scene.Ens.Reflect qualified as Ens
import Move.Scene.Initialize qualified as Initialize
import Move.Scene.Load qualified as Load
import Move.Scene.Module.GetEnabledPreset qualified as Module
import Move.Scene.Parse qualified as Parse
import Move.Scene.Parse.Core (Handle (Handle))
import Move.Scene.Parse.Core qualified as P
import Move.Scene.Parse.Program qualified as Parse
import Move.Scene.Unravel qualified as Unravel
import Path
import Rule.Ens.Reify qualified as Ens
import Rule.FileType qualified as FT
import Rule.Module (MainModule (MainModule))
import Rule.RawProgram.Decode qualified as RawProgram
import Rule.Target
import Prelude hiding (log)

format :: ShouldMinimizeImports -> FT.FileType -> Path Abs File -> T.Text -> App T.Text
format shouldMinimizeImports fileType path content = do
  case fileType of
    FT.Ens -> do
      counter <- asks App.counter
      let h = Ens.Handle {counter}
      ens <- toApp $ Ens.fromFilePath' h path content
      return $ Ens.pp ens
    FT.Source -> do
      _formatSource shouldMinimizeImports path content

type ShouldMinimizeImports =
  Bool

_formatSource :: ShouldMinimizeImports -> Path Abs File -> T.Text -> App T.Text
_formatSource shouldMinimizeImports filePath fileContent = do
  Initialize.initializeForTarget
  MainModule mainModule <- getMainModule
  if shouldMinimizeImports
    then do
      h <- Unravel.new
      (_, dependenceSeq) <- Unravel.unravel h mainModule $ Main (emptyZen filePath)
      h' <- Load.new
      contentSeq <- toApp $ Load.load h' Peripheral dependenceSeq
      let contentSeq' = _replaceLast fileContent contentSeq
      forM_ contentSeq' $ \(source, cacheOrContent) -> do
        Initialize.initializeForSource source
        void $ Parse.parse Peripheral source cacheOrContent
      unusedGlobalLocators <- UnusedGlobalLocator.get
      unusedLocalLocators <- UnusedLocalLocator.get
      counter <- asks App.counter
      let h'' = Handle {counter, filePath, fileContent, mustParseWholeFile = True}
      program <- toApp $ P.parseFile h'' Parse.parseProgram
      hMod <- Module.new
      presetNames <- toApp $ Module.getEnabledPreset hMod mainModule
      let importInfo = RawProgram.ImportInfo {presetNames, unusedGlobalLocators, unusedLocalLocators}
      return $ RawProgram.pp importInfo program
    else do
      counter <- asks App.counter
      let h = Handle {counter, filePath, fileContent, mustParseWholeFile = True}
      program <- toApp $ P.parseFile h Parse.parseProgram
      hMod <- Module.new
      presetNames <- toApp $ Module.getEnabledPreset hMod mainModule
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
