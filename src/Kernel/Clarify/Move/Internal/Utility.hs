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
toAffineApp :: Handle -> C.Value -> C.Comp -> IO C.Comp
toAffineApp h v t = do
  (expVarName, expVar) <- Gensym.createVar (gensymHandle h) "exp"
  return $ C.UpElim True expVarName t (C.PiElimDownElim expVar [C.Int (IntSize (baseSize h)) 0, v])

-- toRelevantApp meta x t ~>
--   bind exp := t in
--   exp @ (1, x)
toRelevantApp :: Handle -> C.Value -> C.Comp -> IO C.Comp
toRelevantApp h v t = do
  (expVarName, expVar) <- Gensym.createVar (gensymHandle h) "exp"
  return $ C.UpElim True expVarName t (C.PiElimDownElim expVar [C.Int (IntSize (baseSize h)) 1, v])

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
  defaultLabel <- Gensym.newIdentFromText (gensymHandle h) "default"
  defaultBranch' <- Subst.subst (substHandle h) sub defaultBranch
  let (tags, clauses) = unzip branchList
  labels <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "case") clauses
  clauses' <- mapM (Subst.subst (substHandle h) sub) clauses
  let defaultClause = (defaultLabel, defaultBranch')
  let clauseList = zip labels clauses'
  goalLabel <- Gensym.newIdentFromText (gensymHandle h) "goal"
  defaultClause' <- adjustBranch h goalLabel defaultClause
  clauseList' <- mapM (adjustBranch h goalLabel) clauseList
  resultVar <- Gensym.newIdentFromText (gensymHandle h) "result"
  return $ C.EnumElim newToOld d defaultClause' (zip tags clauseList') [resultVar] goalLabel $ C.UpIntro (C.VarLocal resultVar)

adjustBranch :: Handle -> C.Label -> (C.Label, C.Comp) -> IO (C.Label, C.Comp)
adjustBranch h goalLabel (label, body) = do
  (phiVarName, phiVar) <- Gensym.createVar (gensymHandle h) "phi"
  return (label, C.UpElim False phiVarName body $ C.Phi goalLabel [phiVar])

getSub :: Gensym.Handle -> [Ident] -> IO ([(Int, C.Value)], [(Int, C.Value)])
getSub h idents = do
  newIdents <- mapM (Gensym.newIdentFromIdent h) idents
  let newToOld = zip (map toInt newIdents) (map C.VarLocal idents)
  let oldToNew = zip (map toInt idents) (map C.VarLocal newIdents)
  return (newToOld, oldToNew)
