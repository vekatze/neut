module Kernel.Clarify.Move.Internal.Utility
  ( Handle,
    new,
    toAffineApp,
    toRelevantApp,
    bindLet,
    bindLetWithReducibility,
    irreducibleBindLet,
    registerSwitcher,
    getEnumElim,
  )
where

import Data.IntMap qualified as IntMap
import Gensym.Rule.Handle qualified as Gensym
import Kernel.Clarify.Move.Internal.Handle.AuxEnv qualified as AuxEnv
import Language.Common.Move.CreateSymbol qualified as Gensym
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify
import Language.Common.Rule.Opacity qualified as O
import Language.Common.Rule.PrimNumSize
import Language.Comp.Move.CreateVar qualified as Gensym
import Language.Comp.Move.Subst qualified as Subst
import Language.Comp.Rule.Comp qualified as C
import Language.Comp.Rule.EnumCase
import Language.Comp.Rule.EnumCase qualified as EC

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    substHandle :: Subst.Handle,
    auxEnvHandle :: AuxEnv.Handle,
    baseSize :: Int
  }

new :: Gensym.Handle -> Subst.Handle -> AuxEnv.Handle -> Int -> Handle
new gensymHandle substHandle auxEnvHandle baseSize = do
  Handle {..}

-- toAffineApp meta x t ~>
--   bind exp := t in
--   exp @ (0, x)
toAffineApp :: Handle -> Ident -> C.Comp -> IO C.Comp
toAffineApp h x t = do
  (expVarName, expVar) <- Gensym.createVar (gensymHandle h) "exp"
  return $ C.UpElim True expVarName t (C.PiElimDownElim expVar [C.Int (IntSize (baseSize h)) 0, C.VarLocal x])

-- toRelevantApp meta x t ~>
--   bind exp := t in
--   exp @ (1, x)
toRelevantApp :: Handle -> Ident -> C.Comp -> IO C.Comp
toRelevantApp h x t = do
  (expVarName, expVar) <- Gensym.createVar (gensymHandle h) "exp"
  return $ C.UpElim True expVarName t (C.PiElimDownElim expVar [C.Int (IntSize (baseSize h)) 1, C.VarLocal x])

bindLet :: [(Ident, C.Comp)] -> C.Comp -> C.Comp
bindLet =
  bindLetWithReducibility True

irreducibleBindLet :: [(Ident, C.Comp)] -> C.Comp -> C.Comp
irreducibleBindLet =
  bindLetWithReducibility False

bindLetWithReducibility :: C.IsReducible -> [(Ident, C.Comp)] -> C.Comp -> C.Comp
bindLetWithReducibility isReducible binder cont =
  case binder of
    [] ->
      cont
    (x, e) : xes ->
      C.UpElim isReducible x e $ bindLetWithReducibility isReducible xes cont

makeSwitcher ::
  Handle ->
  (C.Value -> IO C.Comp) ->
  (C.Value -> IO C.Comp) ->
  IO ([Ident], C.Comp)
makeSwitcher h compAff compRel = do
  (switchVarName, switchVar) <- Gensym.createVar (gensymHandle h) "switch"
  (argVarName, argVar) <- Gensym.createVar (gensymHandle h) "arg"
  aff <- compAff argVar
  rel <- compRel argVar
  enumElim <- getEnumElim h [argVarName] switchVar rel [(EC.Int 0, aff)]
  return ([switchVarName, argVarName], enumElim)

registerSwitcher ::
  Handle ->
  O.Opacity ->
  DD.DefiniteDescription ->
  (C.Value -> IO C.Comp) ->
  (C.Value -> IO C.Comp) ->
  IO ()
registerSwitcher h opacity name aff rel = do
  (args, e) <- makeSwitcher h aff rel
  AuxEnv.insert (auxEnvHandle h) name (opacity, args, e)

getEnumElim :: Handle -> [Ident] -> C.Value -> C.Comp -> [(EnumCase, C.Comp)] -> IO C.Comp
getEnumElim h idents d defaultBranch branchList = do
  (newToOld, oldToNew) <- getSub (gensymHandle h) idents
  let sub = IntMap.fromList oldToNew
  defaultBranch' <- Subst.subst (substHandle h) sub defaultBranch
  let (labels, clauses) = unzip branchList
  clauses' <- mapM (Subst.subst (substHandle h) sub) clauses
  return $ C.EnumElim newToOld d defaultBranch' (zip labels clauses')

getSub :: Gensym.Handle -> [Ident] -> IO ([(Int, C.Value)], [(Int, C.Value)])
getSub h idents = do
  newIdents <- mapM (Gensym.newIdentFromIdent h) idents
  let newToOld = zip (map toInt newIdents) (map C.VarLocal idents)
  let oldToNew = zip (map toInt idents) (map C.VarLocal newIdents)
  return (newToOld, oldToNew)
