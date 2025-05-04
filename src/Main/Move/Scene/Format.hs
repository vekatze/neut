module Main.Move.Scene.Format
  ( Handle,
    new,
    format,
    ShouldMinimizeImports,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Main.Move.Context.EIO (EIO)
import Main.Move.Context.Env qualified as Env
import Main.Move.Scene.Ens.Reflect qualified as EnsReflect
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Init.Local qualified as Local
import Main.Move.Scene.Load qualified as Load
import Main.Move.Scene.Module.GetEnabledPreset qualified as GetEnabledPreset
import Main.Move.Scene.Parse qualified as Parse
import Main.Move.Scene.Parse.Core qualified as ParseCore
import Main.Move.Scene.Parse.Program qualified as Parse
import Main.Move.Scene.Unravel qualified as Unravel
import Main.Rule.Ens.Reify qualified as Ens
import Main.Rule.FileType qualified as FT
import Main.Rule.Module (MainModule (MainModule))
import Main.Rule.RawProgram.Decode (ImportInfo (unusedGlobalLocators, unusedLocalLocators))
import Main.Rule.RawProgram.Decode qualified as RawProgram
import Main.Rule.Target
import Path
import Prelude hiding (log)

newtype Handle = Handle
  { baseHandle :: Base.Handle
  }

new ::
  Base.Handle ->
  Handle
new baseHandle = do
  Handle {..}

format :: Handle -> ShouldMinimizeImports -> FT.FileType -> Path Abs File -> T.Text -> EIO T.Text
format h shouldMinimizeImports fileType path content = do
  case fileType of
    FT.Ens -> do
      let ensReflectHandle = EnsReflect.new (Base.gensymHandle (baseHandle h))
      ens <- EnsReflect.fromFilePath' ensReflectHandle path content
      return $ Ens.pp ens
    FT.Source -> do
      _formatSource h shouldMinimizeImports path content

type ShouldMinimizeImports =
  Bool

_formatSource :: Handle -> ShouldMinimizeImports -> Path Abs File -> T.Text -> EIO T.Text
_formatSource h shouldMinimizeImports filePath fileContent = do
  let MainModule mainModule = Env.getMainModule (Base.envHandle (baseHandle h))
  let parseCoreHandle = ParseCore.new (Base.gensymHandle (baseHandle h))
  let getEnabledPresetHandle = GetEnabledPreset.new (baseHandle h)
  if shouldMinimizeImports
    then do
      unravelHandle <- liftIO $ Unravel.new (baseHandle h)
      let loadHandle = Load.new (baseHandle h)
      (_, dependenceSeq) <- Unravel.unravel unravelHandle mainModule $ Main (emptyZen filePath)
      contentSeq <- Load.load loadHandle Peripheral dependenceSeq
      case unsnoc contentSeq of
        Nothing ->
          undefined
        Just (headItems, (rootSource, __)) -> do
          forM_ headItems $ \(source, cacheOrContent) -> do
            localHandle <- Local.new (baseHandle h) source
            let parseHandle = Parse.new (baseHandle h) localHandle
            void $ Parse.parse parseHandle Peripheral source cacheOrContent
          localHandle <- Local.new (baseHandle h) rootSource
          let parseHandle = Parse.new (baseHandle h) localHandle
          void $ Parse.parse parseHandle Peripheral rootSource (Right fileContent)
          (unusedGlobalLocators, unusedLocalLocators) <- liftIO $ Parse.getUnusedLocators parseHandle
          program <- ParseCore.parseFile parseCoreHandle filePath fileContent True Parse.parseProgram
          presetNames <- GetEnabledPreset.getEnabledPreset getEnabledPresetHandle mainModule
          let importInfo = RawProgram.ImportInfo {presetNames, unusedGlobalLocators, unusedLocalLocators}
          return $ RawProgram.pp importInfo program
    else do
      program <- ParseCore.parseFile parseCoreHandle filePath fileContent True Parse.parseProgram
      presetNames <- GetEnabledPreset.getEnabledPreset getEnabledPresetHandle mainModule
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

unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
