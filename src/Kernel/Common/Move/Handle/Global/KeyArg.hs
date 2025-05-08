module Kernel.Common.Move.Handle.Global.KeyArg
  ( new,
    insert,
    lookup,
    reorderArgs,
  )
where

import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Kernel.Common.Rule.Handle.Global.KeyArg
import Kernel.Common.Rule.Module
import Language.Common.Move.Raise (raiseError)
import Language.Common.Rule.ArgNum qualified as AN
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.IsConstLike
import Language.RawTerm.Rule.Key
import Library.Error.Rule.EIO (EIO)
import Library.Logger.Rule.Hint
import Prelude hiding (lookup, read)

new :: MainModule -> IO Handle
new _mainModule = do
  _keyArgMapRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> Hint -> DD.DefiniteDescription -> IsConstLike -> AN.ArgNum -> [Key] -> EIO ()
insert h m funcName isConstLike argNum keys = do
  kmap <- liftIO $ readIORef (_keyArgMapRef h)
  case Map.lookup funcName kmap of
    Nothing ->
      return ()
    Just (isConstLike', (argNum', keys'))
      | isConstLike,
        not isConstLike' -> do
          let funcName' = DD.getReadableDD (_mainModule h) funcName
          raiseError m $
            "`"
              <> funcName'
              <> "` is declared as a function, but defined as a constant-like term."
      | not isConstLike,
        isConstLike' -> do
          let funcName' = DD.getReadableDD (_mainModule h) funcName
          raiseError m $
            "`"
              <> funcName'
              <> "` is declared as a constant-like term, but defined as a function."
      | argNum /= argNum' -> do
          let funcName' = DD.getReadableDD (_mainModule h) funcName
          raiseError m $
            "The arity of `"
              <> funcName'
              <> "` is declared as "
              <> T.pack (show $ AN.reify argNum')
              <> ", but defined as "
              <> T.pack (show $ AN.reify argNum)
              <> "."
      | not $ _eqKeys keys keys' -> do
          let funcName' = DD.getReadableDD (_mainModule h) funcName
          raiseError m $
            "The explicit key sequence of `"
              <> funcName'
              <> "` is declared as `"
              <> _showKeys keys'
              <> "`, but defined as `"
              <> _showKeys keys
              <> "`."
      | otherwise ->
          return ()
  liftIO $ modifyIORef' (_keyArgMapRef h) $ Map.insert funcName (isConstLike, (argNum, keys))

lookup :: Handle -> Hint -> DD.DefiniteDescription -> EIO (AN.ArgNum, [Key])
lookup h m dataName = do
  keyArgMap <- liftIO $ readIORef (_keyArgMapRef h)
  case Map.lookup dataName keyArgMap of
    Just (_, value) ->
      return value
    Nothing -> do
      let dataName' = DD.getReadableDD (_mainModule h) dataName
      raiseError m $ "No such function is defined: " <> dataName'

reorderArgs :: Hint -> [Key] -> Map.HashMap Key a -> EIO [a]
reorderArgs m keyList kvs =
  case keyList of
    []
      | Map.null kvs ->
          return []
      | otherwise -> do
          let ks = map fst $ Map.toList kvs
          raiseError m $ "The following fields are redundant:\n" <> _showKeyList ks
    key : keyRest
      | Just v <- Map.lookup key kvs -> do
          vs <- reorderArgs m keyRest (Map.delete key kvs)
          return $ v : vs
      | otherwise ->
          raiseError m $ "The field `" <> key <> "` is missing"
