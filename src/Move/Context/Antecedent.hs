module Move.Context.Antecedent
  ( initialize,
    setMap,
    getMap,
    RevMap,
    getReverseMap,
    lookup,
    getShiftDigest,
  )
where

import Move.Context.App
import Move.Context.App.Internal
import Control.Monad
import Data.ByteString.UTF8 qualified as B
import Data.HashMap.Strict qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Rule.Digest (hashAndEncode)
import Rule.Module qualified as M
import Rule.ModuleDigest qualified as MD
import Rule.ModuleID qualified as MID
import Prelude hiding (lookup, read)

initialize :: App ()
initialize = do
  writeRef' antecedentMap Map.empty
  writeRef' reverseAntecedentMap Map.empty
  writeRef' antecedentDigestCache Nothing

setMap :: Map.HashMap MID.ModuleID M.Module -> App ()
setMap mp = do
  writeRef' antecedentMap mp
  forM_ (Map.toList mp) $ \(mid, m) -> do
    modifyRef' reverseAntecedentMap $ Map.insertWith S.union (M.moduleID m) (S.singleton mid)

getMap :: App (Map.HashMap MID.ModuleID M.Module)
getMap =
  readRef' antecedentMap

type RevMap =
  Map.HashMap MID.ModuleID (S.Set MID.ModuleID)

getReverseMap :: App RevMap
getReverseMap =
  readRef' reverseAntecedentMap

lookup :: MD.ModuleDigest -> App (Maybe M.Module)
lookup mc = do
  aenv <- readRef' antecedentMap
  return $ Map.lookup (MID.Library mc) aenv

getShiftDigest :: App T.Text
getShiftDigest = do
  digestOrNone <- readRef' antecedentDigestCache
  case digestOrNone of
    Just digest -> do
      return digest
    Nothing -> do
      amap <- Map.toList <$> getMap
      let amap' = map (\(foo, bar) -> (MID.reify foo, MID.reify $ M.moduleID bar)) amap
      let digest = T.pack $ B.toString $ hashAndEncode $ B.fromString $ show amap'
      writeRef antecedentDigestCache digest
      return digest
