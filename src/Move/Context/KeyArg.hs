module Move.Context.KeyArg
  ( insert,
    lookup,
    lookupMaybe,
    reorderArgs,
  )
where

import Move.Context.App
import Move.Context.App.Internal
import Move.Context.Locator qualified as Locator
import Move.Context.Throw qualified as Throw
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Rule.ArgNum qualified as AN
import Rule.Const (holeVarPrefix)
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.IsConstLike
import Rule.Key
import Prelude hiding (lookup, read)

insert :: Hint -> DD.DefiniteDescription -> IsConstLike -> AN.ArgNum -> [Key] -> App ()
insert m funcName isConstLike argNum keys = do
  kmap <- readRef' keyArgMap
  case Map.lookup funcName kmap of
    Nothing ->
      return ()
    Just (isConstLike', (argNum', keys'))
      | isConstLike,
        not isConstLike' -> do
          funcName' <- Locator.getReadableDD funcName
          Throw.raiseError m $
            "`"
              <> funcName'
              <> "` is declared as a function, but defined as a constant-like term."
      | not isConstLike,
        isConstLike' -> do
          funcName' <- Locator.getReadableDD funcName
          Throw.raiseError m $
            "`"
              <> funcName'
              <> "` is declared as a constant-like term, but defined as a function."
      | argNum /= argNum' -> do
          funcName' <- Locator.getReadableDD funcName
          Throw.raiseError m $
            "The arity of `"
              <> funcName'
              <> "` is declared as "
              <> T.pack (show $ AN.reify argNum')
              <> ", but defined as "
              <> T.pack (show $ AN.reify argNum)
              <> "."
      | not $ eqKeys keys keys' -> do
          funcName' <- Locator.getReadableDD funcName
          Throw.raiseError m $
            "The explicit key sequence of `"
              <> funcName'
              <> "` is declared as `"
              <> showKeys keys'
              <> "`, but defined as `"
              <> showKeys keys
              <> "`."
      | otherwise ->
          return ()
  modifyRef' keyArgMap $ Map.insert funcName (isConstLike, (argNum, keys))

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

lookup :: Hint -> DD.DefiniteDescription -> App (AN.ArgNum, [Key])
lookup m dataName = do
  mValue <- Map.lookup dataName <$> readRef' keyArgMap
  case mValue of
    Just (_, value) ->
      return value
    Nothing -> do
      dataName' <- Locator.getReadableDD dataName
      Throw.raiseError m $ "No such function is defined: " <> dataName'

lookupMaybe :: DD.DefiniteDescription -> App (Maybe (IsConstLike, [Key]))
lookupMaybe dataName = do
  mValue <- Map.lookup dataName <$> readRef' keyArgMap
  case mValue of
    Just (isConstLike, (_, keyList)) ->
      return $ Just (isConstLike, keyList)
    _ ->
      return Nothing

reorderArgs :: Hint -> [Key] -> Map.HashMap Key a -> App [a]
reorderArgs m keyList kvs =
  case keyList of
    []
      | Map.null kvs ->
          return []
      | otherwise -> do
          let ks = map fst $ Map.toList kvs
          Throw.raiseError m $ "The following fields are redundant:\n" <> showKeyList ks
    key : keyRest
      | Just v <- Map.lookup key kvs -> do
          vs <- reorderArgs m keyRest (Map.delete key kvs)
          return $ v : vs
      | otherwise ->
          Throw.raiseError m $ "The field `" <> key <> "` is missing"

showKeyList :: [Key] -> T.Text
showKeyList ks =
  T.intercalate "\n" $ map ("- " <>) ks
