module Command.Common.Move.Build.EnsureMain
  ( Handle,
    new,
    ensureMain,
  )
where

import Control.Monad
import Data.Text qualified as T
import Error.Rule.EIO (EIO)
import Kernel.Move.Context.Env qualified as Env
import Kernel.Common.Rule.Module
import Kernel.Common.Rule.Source
import Kernel.Common.Rule.Target
import Language.Common.Move.Raise (raiseError)
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.ModuleID qualified as MID
import Logger.Rule.Hint

newtype Handle = Handle
  { envHandle :: Env.Handle
  }

new :: Env.Handle -> Handle
new envHandle = do
  Handle {..}

ensureMain :: Handle -> Target -> Source -> [DD.DefiniteDescription] -> EIO ()
ensureMain h t source topLevelNameList = do
  case t of
    Main target -> do
      mainDD <- Env.getMainDefiniteDescriptionByTarget (envHandle h) target
      let hasEntryPoint = mainDD `elem` topLevelNameList
      let entryPointIsNecessary = checkIfEntryPointIsNecessary target source
      when (entryPointIsNecessary && not hasEntryPoint) $ do
        let entryPointName = getEntryPointName target
        let m = newSourceHint $ sourceFilePath source
        raiseMissingEntryPoint m (BN.reify entryPointName)
    Peripheral {} ->
      return ()
    PeripheralSingle {} ->
      return ()

type EntryPointName =
  T.Text

raiseMissingEntryPoint :: Hint -> EntryPointName -> EIO a
raiseMissingEntryPoint m entryPointName = do
  raiseError m $ "`" <> entryPointName <> "` is missing"

checkIfEntryPointIsNecessary :: MainTarget -> Source -> Bool
checkIfEntryPointIsNecessary target source = do
  case target of
    Named {} -> do
      isMainFile source
    Zen path _ -> do
      sourceFilePath source == path

isMainFile :: Source -> Bool
isMainFile source = do
  case moduleID $ sourceModule source of
    MID.Main -> do
      let sourcePathList = getTargetPathList $ sourceModule source
      sourceFilePath source `elem` sourcePathList
    _ ->
      False
