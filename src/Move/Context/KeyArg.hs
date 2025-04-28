module Move.Context.KeyArg
  ( Handle,
    new,
    insert,
    lookup,
    lookupMaybe,
    reorderArgs,
  )
where

import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Move.Context.EIO (EIO, raiseError)
import Move.Context.Env qualified as Env
import Move.Context.Locator qualified as Locator
import Rule.ArgNum qualified as AN
import Rule.Const (holeVarPrefix)
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.IsConstLike
import Rule.Key
import Prelude hiding (lookup, read)

data Handle
  = Handle
  { envHandle :: Env.Handle,
    keyArgMapRef :: IORef (Map.HashMap DD.DefiniteDescription (IsConstLike, (AN.ArgNum, [Key])))
  }

new :: Env.Handle -> IO Handle
new envHandle = do
  keyArgMapRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> Hint -> DD.DefiniteDescription -> IsConstLike -> AN.ArgNum -> [Key] -> EIO ()
insert h m funcName isConstLike argNum keys = do
  kmap <- liftIO $ readIORef (keyArgMapRef h)
  mainModule <- Env.getMainModule (envHandle h)
  case Map.lookup funcName kmap of
    Nothing ->
      return ()
    Just (isConstLike', (argNum', keys'))
      | isConstLike,
        not isConstLike' -> do
          let funcName' = Locator.getReadableDD mainModule funcName
          raiseError m $
            "`"
              <> funcName'
              <> "` is declared as a function, but defined as a constant-like term."
      | not isConstLike,
        isConstLike' -> do
          let funcName' = Locator.getReadableDD mainModule funcName
          raiseError m $
            "`"
              <> funcName'
              <> "` is declared as a constant-like term, but defined as a function."
      | argNum /= argNum' -> do
          let funcName' = Locator.getReadableDD mainModule funcName
          raiseError m $
            "The arity of `"
              <> funcName'
              <> "` is declared as "
              <> T.pack (show $ AN.reify argNum')
              <> ", but defined as "
              <> T.pack (show $ AN.reify argNum)
              <> "."
      | not $ eqKeys keys keys' -> do
          let funcName' = Locator.getReadableDD mainModule funcName
          raiseError m $
            "The explicit key sequence of `"
              <> funcName'
              <> "` is declared as `"
              <> showKeys keys'
              <> "`, but defined as `"
              <> showKeys keys
              <> "`."
      | otherwise ->
          return ()
  liftIO $ modifyIORef' (keyArgMapRef h) $ Map.insert funcName (isConstLike, (argNum, keys))

isHole :: Key -> Bool
isHole =
  T.isPrefixOf holeVarPrefix

eqKeys :: [Key] -> [Key] -> Bool
eqKeys ks1 ks2 =
  case (ks1, ks2) of
    ([], []) ->
      True
    (_ : _, []) ->
      False
    ([], _ : _) ->
      False
    (k1 : rest1, k2 : rest2) ->
      case (isHole k1, isHole k2) of
        (True, True) ->
          eqKeys rest1 rest2
        (True, False) ->
          False
        (False, True) ->
          False
        (False, False)
          | k1 == k2 ->
              eqKeys rest1 rest2
          | otherwise ->
              False

showKeys :: [Key] -> T.Text
showKeys keys =
  case keys of
    [] ->
      "(empty)"
    [k] ->
      showKey k
    k : rest ->
      showKey k <> ", " <> showKeys rest

showKey :: Key -> T.Text
showKey k =
  if isHole k
    then "_"
    else k

lookup :: Handle -> Hint -> DD.DefiniteDescription -> EIO (AN.ArgNum, [Key])
lookup h m dataName = do
  keyArgMap <- liftIO $ readIORef (keyArgMapRef h)
  case Map.lookup dataName keyArgMap of
    Just (_, value) ->
      return value
    Nothing -> do
      mainModule <- Env.getMainModule (envHandle h)
      let dataName' = Locator.getReadableDD mainModule dataName
      raiseError m $ "No such function is defined: " <> dataName'

lookupMaybe :: Handle -> DD.DefiniteDescription -> IO (Maybe (IsConstLike, [Key]))
lookupMaybe h dataName = do
  keyArgMap <- liftIO $ readIORef (keyArgMapRef h)
  case Map.lookup dataName keyArgMap of
    Just (isConstLike, (_, keyList)) ->
      return $ Just (isConstLike, keyList)
    _ ->
      return Nothing

reorderArgs :: Hint -> [Key] -> Map.HashMap Key a -> EIO [a]
reorderArgs m keyList kvs =
  case keyList of
    []
      | Map.null kvs ->
          return []
      | otherwise -> do
          let ks = map fst $ Map.toList kvs
          raiseError m $ "The following fields are redundant:\n" <> showKeyList ks
    key : keyRest
      | Just v <- Map.lookup key kvs -> do
          vs <- reorderArgs m keyRest (Map.delete key kvs)
          return $ v : vs
      | otherwise ->
          raiseError m $ "The field `" <> key <> "` is missing"

showKeyList :: [Key] -> T.Text
showKeyList ks =
  T.intercalate "\n" $ map ("- " <>) ks
