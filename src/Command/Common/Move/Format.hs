module Command.Common.Move.Format
  ( Handle,
    new,
    formatSource,
    formatEns,
    ShouldMinimizeImports,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Move.CreateLocalHandle qualified as Local
import Kernel.Common.Move.Module.GetEnabledPreset qualified as GetEnabledPreset
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Kernel.Common.Rule.Module (MainModule (MainModule))
import Kernel.Common.Rule.Target
import Kernel.Load.Move.Load qualified as Load
import Kernel.Parse.Move.Internal.Program qualified as Parse
import Kernel.Parse.Move.Internal.RawTerm qualified as ParseRT
import Kernel.Parse.Move.Parse qualified as Parse
import Kernel.Unravel.Move.Unravel qualified as Unravel
import Language.Common.Move.Raise (raiseError')
import Language.RawTerm.Rule.RawStmt.ToDoc (ImportInfo (unusedGlobalLocators, unusedLocalLocators))
import Language.RawTerm.Rule.RawStmt.ToDoc qualified as RawProgram
import Library.CodeParser.Move.Parse (runParser)
import Library.Ens.Move.Parse qualified as EnsParse
import Library.Ens.Rule.Ens.ToDoc qualified as Ens
import Library.Error.Rule.EIO (EIO)
import Path
import Prelude hiding (log)

newtype Handle = Handle
  { globalHandle :: Global.Handle
  }

new ::
  Global.Handle ->
  Handle
new globalHandle = do
  Handle {..}

formatSource :: Handle -> ShouldMinimizeImports -> Path Abs File -> T.Text -> EIO T.Text
formatSource h shouldMinimizeImports path content = do
  _formatSource h shouldMinimizeImports path content

formatEns :: Path Abs File -> T.Text -> EIO T.Text
formatEns path content = do
  ens <- EnsParse.fromFilePath' path content
  return $ Ens.pp ens

type ShouldMinimizeImports =
  Bool

_formatSource :: Handle -> ShouldMinimizeImports -> Path Abs File -> T.Text -> EIO T.Text
_formatSource h shouldMinimizeImports filePath fileContent = do
  let MainModule mainModule = Env.getMainModule (Global.envHandle (globalHandle h))
  let parseCoreHandle = ParseRT.new (Global.gensymHandle (globalHandle h))
  let getEnabledPresetHandle = GetEnabledPreset.new (globalHandle h)
  if shouldMinimizeImports
    then do
      unravelHandle <- liftIO $ Unravel.new (globalHandle h)
      let loadHandle = Load.new (globalHandle h)
      (_, dependenceSeq) <- Unravel.unravel unravelHandle mainModule $ Main (emptyZen filePath)
      contentSeq <- Load.load loadHandle Peripheral dependenceSeq
      case unsnoc contentSeq of
        Nothing ->
          raiseError' "Nothing to format"
        Just (headItems, (rootSource, __)) -> do
          forM_ headItems $ \(source, cacheOrContent) -> do
            localHandle <- Local.new (globalHandle h) source
            parseHandle <- liftIO $ Parse.new (globalHandle h) localHandle
            void $ Parse.parse parseHandle Peripheral source cacheOrContent
          localHandle <- Local.new (globalHandle h) rootSource
          parseHandle <- liftIO $ Parse.new (globalHandle h) localHandle
          void $ Parse.parse parseHandle Peripheral rootSource (Right fileContent)
          (unusedGlobalLocators, unusedLocalLocators) <- liftIO $ Parse.getUnusedLocators parseHandle
          program <- runParser filePath fileContent True (Parse.parseProgram parseCoreHandle)
          presetNames <- GetEnabledPreset.getEnabledPreset getEnabledPresetHandle mainModule
          let importInfo = RawProgram.ImportInfo {presetNames, unusedGlobalLocators, unusedLocalLocators}
          return $ RawProgram.pp importInfo program
    else do
      program <- runParser filePath fileContent True (Parse.parseProgram parseCoreHandle)
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
