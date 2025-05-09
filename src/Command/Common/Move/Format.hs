module Command.Common.Move.Format
  ( Handle,
    new,
    formatSource,
    formatEns,
    ShouldMinimizeImports,
  )
where

import Aux.CodeParser.Move.Parse (runParser)
import Aux.Ens.Move.Parse qualified as EnsParse
import Aux.Ens.Rule.Ens.ToDoc qualified as Ens
import Aux.Error.Move.Run (raiseError')
import Aux.Error.Rule.EIO (EIO)
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
import Kernel.Parse.Move.Internal.Handle.Unused qualified as Unused
import Kernel.Parse.Move.Internal.Program qualified as Parse
import Kernel.Parse.Move.Internal.RawTerm qualified as ParseRT
import Kernel.Parse.Move.Interpret qualified as Interpret
import Kernel.Parse.Move.Parse qualified as Parse
import Kernel.Unravel.Move.Unravel qualified as Unravel
import Language.RawTerm.Rule.RawStmt.ToDoc (ImportInfo (unusedGlobalLocators, unusedLocalLocators))
import Language.RawTerm.Rule.RawStmt.ToDoc qualified as RawProgram
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
      cacheOrProgList <- Parse.parse (globalHandle h) (_replaceLast fileContent contentSeq)
      case unsnoc cacheOrProgList of
        Nothing ->
          raiseError' "Nothing to format"
        Just (_, (rootLocalHandle, (rootSource, rootCacheOrProg))) -> do
          interpretHandle <- liftIO $ Interpret.new (globalHandle h) rootLocalHandle
          _ <- Interpret.interpret interpretHandle Peripheral rootSource rootCacheOrProg
          let unusedHandle = Local.unusedHandle rootLocalHandle
          (unusedGlobalLocators, unusedLocalLocators) <- liftIO $ Unused.getUnusedLocators unusedHandle
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
