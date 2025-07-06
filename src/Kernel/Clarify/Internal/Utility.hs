module Kernel.Clarify.Internal.Utility
  ( Handle,
    ResourceSpec (..),
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
import Gensym.Handle qualified as Gensym
import Kernel.Clarify.Internal.Handle.AuxEnv qualified as AuxEnv
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DataSize qualified as DS
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident
import Language.Common.Ident.Reify
import Language.Common.Opacity qualified as O
import Language.Common.PrimNumSize
import Language.Comp.Comp qualified as C
import Language.Comp.CreateVar qualified as Gensym
import Language.Comp.EnumCase
import Language.Comp.EnumCase qualified as EC
import Language.Comp.Subst qualified as Subst

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    substHandle :: Subst.Handle,
    auxEnvHandle :: AuxEnv.Handle,
    baseSize :: DS.DataSize
  }

new :: Gensym.Handle -> Subst.Handle -> AuxEnv.Handle -> DS.DataSize -> Handle
new gensymHandle substHandle auxEnvHandle baseSize = do
  Handle {..}

-- toAffineApp meta x t ~>
--   bind exp := t in
--   exp @ (0, x)
toAffineApp :: Handle -> C.Value -> C.Comp -> IO C.Comp
toAffineApp h v t = do
  (expVarName, expVar) <- Gensym.createVar (gensymHandle h) "exp"
  return $ C.UpElim True expVarName t (C.PiElimDownElim expVar [C.Int (dataSizeToIntSize (baseSize h)) 0, v])

-- toRelevantApp meta x t ~>
--   bind exp := t in
--   exp @ (1, x)
toRelevantApp :: Handle -> C.Value -> C.Comp -> IO C.Comp
toRelevantApp h v t = do
  (expVarName, expVar) <- Gensym.createVar (gensymHandle h) "exp"
  return $ C.UpElim True expVarName t (C.PiElimDownElim expVar [C.Int (dataSizeToIntSize (baseSize h)) 1, v])

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
  ResourceSpec ->
  IO ([Ident], C.Comp)
makeSwitcher h resourceSpec = do
  let ResourceSpec {defaultClause, clauses} = resourceSpec
  let (argVarName, _) = arg resourceSpec
  let (switchVarName, switchVar) = switch resourceSpec
  let clauseList = zip (map EC.Int [0 ..]) clauses
  enumElim <- getEnumElim h [argVarName] switchVar defaultClause clauseList
  return ([switchVarName, argVarName], enumElim)

data ResourceSpec = ResourceSpec
  { switch :: (Ident, C.Value),
    arg :: (Ident, C.Value),
    defaultClause :: C.Comp,
    clauses :: [C.Comp]
  }

registerSwitcher ::
  Handle ->
  O.Opacity ->
  DD.DefiniteDescription ->
  ResourceSpec ->
  IO ()
registerSwitcher h opacity name resourceSpec = do
  (args, e) <- makeSwitcher h resourceSpec
  AuxEnv.insert (auxEnvHandle h) name (opacity, args, e)

getEnumElim :: Handle -> [Ident] -> C.Value -> C.Comp -> [(EnumCase, C.Comp)] -> IO C.Comp
getEnumElim h idents d defaultBranch branchList = do
  case prune defaultBranch branchList of
    Nothing ->
      return C.Unreachable
    Just (defaultBranch', branchList') -> do
      (newToOld, oldToNew) <- getSub (gensymHandle h) idents
      let sub = IntMap.fromList oldToNew
      defaultBranch'' <- Subst.subst (substHandle h) sub defaultBranch'
      let (tags, clauses) = unzip branchList'
      clauses' <- mapM (Subst.subst (substHandle h) sub) clauses
      defaultClause' <- adjustBranch h defaultBranch''
      clauseList' <- mapM (adjustBranch h) clauses'
      resultVar <- Gensym.newIdentFromText (gensymHandle h) "result"
      return $ C.EnumElim newToOld d defaultClause' (zip tags clauseList') [resultVar] $ C.UpIntro (C.VarLocal resultVar)

adjustBranch :: Handle -> C.Comp -> IO C.Comp
adjustBranch h body = do
  (phiVarName, phiVar) <- Gensym.createVar (gensymHandle h) "phi"
  return $ C.UpElim False phiVarName body $ C.Phi [phiVar]

getSub :: Gensym.Handle -> [Ident] -> IO ([(Int, C.Value)], [(Int, C.Value)])
getSub h idents = do
  newIdents <- mapM (Gensym.newIdentFromIdent h) idents
  let newToOld = zip (map toInt newIdents) (map C.VarLocal idents)
  let oldToNew = zip (map toInt idents) (map C.VarLocal newIdents)
  return (newToOld, oldToNew)

prune :: C.Comp -> [(EnumCase, C.Comp)] -> Maybe (C.Comp, [(EnumCase, C.Comp)])
prune defaultBranch branchList = do
  let branchList' = filter (\(_, branch) -> not $ C.isUnreachable branch) branchList
  case (C.isUnreachable defaultBranch, branchList') of
    (False, _) ->
      Just (defaultBranch, branchList')
    (True, []) ->
      Nothing
    (True, (_, b) : rest) ->
      Just (b, rest)
