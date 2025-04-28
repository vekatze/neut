module Move.Scene.Clarify.Utility
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
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Clarify.Handle.AuxEnv qualified as AuxEnv
import Move.Scene.Comp.Subst qualified as Subst
import Rule.Comp qualified as C
import Rule.DefiniteDescription qualified as DD
import Rule.EnumCase
import Rule.EnumCase qualified as EC
import Rule.Ident
import Rule.Ident.Reify
import Rule.Opacity qualified as O
import Rule.PrimNumSize

data Handle
  = Handle
  { gensymHandle :: Gensym.Handle,
    substHandle :: Subst.Handle,
    auxEnvHandle :: AuxEnv.Handle,
    baseSize :: Int
  }

new :: Gensym.Handle -> App Handle
new gensymHandle = do
  substHandle <- Subst.new gensymHandle
  auxEnvHandle <- AuxEnv.new
  baseSize <- toApp Env.getBaseSize'
  return $ Handle {..}

-- toAffineApp meta x t ~>
--   bind exp := t in
--   exp @ (0, x)
toAffineApp :: Handle -> Ident -> C.Comp -> IO C.Comp
toAffineApp h x t = do
  (expVarName, expVar) <- Gensym.newValueVarLocalWith (gensymHandle h) "exp"
  return $ C.UpElim True expVarName t (C.PiElimDownElim expVar [C.Int (IntSize (baseSize h)) 0, C.VarLocal x])

-- toRelevantApp meta x t ~>
--   bind exp := t in
--   exp @ (1, x)
toRelevantApp :: Handle -> Ident -> C.Comp -> IO C.Comp
toRelevantApp h x t = do
  (expVarName, expVar) <- Gensym.newValueVarLocalWith (gensymHandle h) "exp"
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
  (switchVarName, switchVar) <- Gensym.newValueVarLocalWith (gensymHandle h) "switch"
  (argVarName, argVar) <- Gensym.newValueVarLocalWith (gensymHandle h) "arg"
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
