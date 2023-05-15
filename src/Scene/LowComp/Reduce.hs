module Scene.LowComp.Reduce (reduce) where

import Context.App
import Context.Gensym qualified as Gensym
import Data.IntMap qualified as IntMap
import Entity.Ident.Reify qualified as Ident
import Entity.LowComp qualified as LC
import Entity.LowComp.Subst

reduce :: SubstLowComp -> LC.Comp -> App LC.Comp
reduce sub lowComp = do
  reduce' sub lowComp

reduce' :: SubstLowComp -> LC.Comp -> App LC.Comp
reduce' sub lowComp = do
  case lowComp of
    LC.Return d ->
      return $ LC.Return $ substLowValue sub d
    LC.Let x op cont ->
      case op of
        LC.Bitcast d from to
          | from == to -> do
              let sub' = IntMap.insert (Ident.toInt x) (substLowValue sub d) sub
              reduce' sub' cont
        _ -> do
          x' <- Gensym.newIdentFromIdent x
          let sub' = IntMap.insert (Ident.toInt x) (LC.VarLocal x') sub
          cont' <- reduce' sub' cont
          return $ LC.Let x' (substOp sub op) cont'
    LC.Cont op cont -> do
      let op' = substOp sub op
      cont' <- reduce' sub cont
      return $ LC.Cont op' cont'
    LC.Switch (d, t) defaultBranch les (phi, cont) -> do
      let d' = substLowValue sub d
      let (ls, es) = unzip les
      defaultBranch' <- reduce' sub defaultBranch
      es' <- mapM (reduce' sub) es
      phi' <- Gensym.newIdentFromIdent phi
      let sub' = IntMap.insert (Ident.toInt phi) (LC.VarLocal phi') sub
      cont' <- reduce' sub' cont
      return $ LC.Switch (d', t) defaultBranch' (zip ls es') (phi', cont')
    LC.TailCall d ds -> do
      let d' = substLowValue sub d
      let ds' = map (substLowValue sub) ds
      return $ LC.TailCall d' ds'
    LC.Unreachable ->
      return LC.Unreachable
