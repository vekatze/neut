module Context.KeyArg
  ( insert,
    lookup,
    lookupMaybe,
  )
where

import Context.App
import Context.App.Internal
import Context.Remark (printNote')
import Context.Throw qualified as Throw
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Const (holeVarPrefix)
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.IsConstLike
import Entity.Key
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
          Throw.raiseError m $
            "`"
              <> DD.reify funcName
              <> "` is declared as a function, but defined as a constant-like term."
      | not isConstLike,
        isConstLike' -> do
          Throw.raiseError m $
            "`"
              <> DD.reify funcName
              <> "` is declared as a constant-like term, but defined as a function."
      | argNum /= argNum' ->
          Throw.raiseError m $
            "the arity of `"
              <> DD.reify funcName
              <> "` is declared as "
              <> T.pack (show $ AN.reify argNum')
              <> ", but defined as "
              <> T.pack (show $ AN.reify argNum)
              <> "."
      | not $ eqKeys keys keys' -> do
          Throw.raiseError m $
            "the explicit key sequence of `"
              <> DD.reify funcName
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
      printNote' $ T.pack (show dataName)
      Throw.raiseError m $ "no such function is defined: " <> DD.reify dataName

lookupMaybe :: DD.DefiniteDescription -> App (Maybe (IsConstLike, [Key]))
lookupMaybe dataName = do
  mValue <- Map.lookup dataName <$> readRef' keyArgMap
  case mValue of
    Just (isConstLike, (_, keyList)) ->
      return $ Just (isConstLike, keyList)
    _ ->
      return Nothing
