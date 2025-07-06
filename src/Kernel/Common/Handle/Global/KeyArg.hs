module Kernel.Common.Handle.Global.KeyArg
  ( Handle,
    ExpKey,
    ImpKey,
    _showKeyList,
    new,
    insert,
    lookup,
  )
where

import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Error.EIO (EIO)
import Error.Run (raiseError)
import Kernel.Common.Module
import Kernel.Common.ReadableDD
import Kernel.Common.RuleHandle.Global.KeyArg (ExpKey, Handle (..), ImpKey, _eqKeys, _showKeyList, _showKeys)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.IsConstLike
import Logger.Hint
import Prelude hiding (lookup, read)

new :: MainModule -> IO Handle
new _mainModule = do
  _keyArgMapRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> Hint -> DD.DefiniteDescription -> IsConstLike -> [ImpKey] -> [ExpKey] -> EIO ()
insert h m funcName isConstLike impKeys expKeys = do
  kmap <- liftIO $ readIORef (_keyArgMapRef h)
  case Map.lookup funcName kmap of
    Nothing ->
      return ()
    Just (isConstLike', (impKeys', expKeys'))
      | isConstLike,
        not isConstLike' -> do
          let funcName' = readableDD (_mainModule h) funcName
          raiseError m $
            "`"
              <> funcName'
              <> "` is declared as a function, but defined as a constant-like term."
      | not isConstLike,
        isConstLike' -> do
          let funcName' = readableDD (_mainModule h) funcName
          raiseError m $
            "`"
              <> funcName'
              <> "` is declared as a constant-like term, but defined as a function."
      | length impKeys /= length impKeys' -> do
          let funcName' = readableDD (_mainModule h) funcName
          raiseError m $
            "The arity of `"
              <> funcName'
              <> "` is declared as "
              <> T.pack (show $ length impKeys')
              <> ", but defined as "
              <> T.pack (show $ length impKeys)
              <> "."
      | not $ _eqKeys expKeys expKeys' -> do
          let funcName' = readableDD (_mainModule h) funcName
          raiseError m $
            "The explicit key sequence of `"
              <> funcName'
              <> "` is declared as `"
              <> _showKeys expKeys'
              <> "`, but defined as `"
              <> _showKeys expKeys
              <> "`."
      | otherwise ->
          return ()
  liftIO $ atomicModifyIORef' (_keyArgMapRef h) $ \mp -> do
    (Map.insert funcName (isConstLike, (impKeys, expKeys)) mp, ())

lookup :: Handle -> Hint -> DD.DefiniteDescription -> EIO ([ImpKey], [ExpKey])
lookup h m dataName = do
  keyArgMap <- liftIO $ readIORef (_keyArgMapRef h)
  case Map.lookup dataName keyArgMap of
    Just (_, impExpKeys) ->
      return impExpKeys
    Nothing -> do
      let dataName' = readableDD (_mainModule h) dataName
      raiseError m $ "No such function is defined: " <> dataName'
