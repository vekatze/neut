module Move.Scene.EnsureMain
  ( Handle,
    new,
    ensureMain,
  )
where

import Control.Monad
import Data.Text qualified as T
import Move.Context.App
import Move.Context.EIO (EIO, raiseError)
import Move.Context.Env qualified as Env
import Move.Context.Locator qualified as Locator
import Rule.BaseName qualified as BN
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.Source
import Rule.Target

newtype Handle
  = Handle
  { locatorHandle :: Locator.Handle
  }

new :: Env.Handle -> App Handle
new envHandle = do
  locatorHandle <- Locator.new envHandle
  return $ Handle {..}

ensureMain :: Handle -> Target -> Source -> [DD.DefiniteDescription] -> EIO ()
ensureMain h t source topLevelNameList = do
  case t of
    Main target -> do
      mainDD <- Locator.getMainDefiniteDescriptionByTarget (locatorHandle h) target
      let hasEntryPoint = mainDD `elem` topLevelNameList
      let entryPointIsNecessary = Locator.checkIfEntryPointIsNecessary target source
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
