module Scene.Parse.Discern.Struct
  ( ensureFieldLinearity,
    resolveField,
    reorderArgs,
  )
where

import Context.App
import Context.Throw qualified as Throw
import Data.HashMap.Strict qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Key
import Entity.Name
import Scene.Parse.Discern.Name (resolveName)

ensureFieldLinearity ::
  Hint ->
  [Key] ->
  S.Set Key ->
  S.Set Key ->
  App ()
ensureFieldLinearity m ks found nonLinear =
  case ks of
    [] ->
      if S.null nonLinear
        then return ()
        else
          Throw.raiseError m $
            "the following fields are defined more than once:\n"
              <> T.intercalate "\n" (map ("- " <>) (S.toList nonLinear))
    k : rest -> do
      if S.member k found
        then ensureFieldLinearity m rest found (S.insert k nonLinear)
        else ensureFieldLinearity m rest (S.insert k found) nonLinear

resolveField :: Hint -> Name -> App DD.DefiniteDescription
resolveField m varOrLocator = do
  (dd, _) <- resolveName m varOrLocator
  return dd

reorderArgs :: Hint -> [Key] -> Map.HashMap Key a -> App [a]
reorderArgs m keyList kvs =
  case keyList of
    []
      | Map.null kvs ->
          return []
      | otherwise -> do
          let ks = map fst $ Map.toList kvs
          Throw.raiseError m $ "the following fields are redundant:\n" <> showKeyList ks
    key : keyRest
      | Just v <- Map.lookup key kvs -> do
          vs <- reorderArgs m keyRest (Map.delete key kvs)
          return $ v : vs
      | otherwise ->
          Throw.raiseError m $ "the field `" <> key <> "` is missing"

showKeyList :: [Key] -> T.Text
showKeyList ks =
  T.intercalate "\n" $ map ("- " <>) ks
