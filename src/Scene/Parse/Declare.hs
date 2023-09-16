module Scene.Parse.Declare (interpretDeclareTree) where

import Context.App
import Context.Env qualified as Env
import Context.Throw (liftEither)
import Entity.DataSize qualified as DS
import Entity.Decl qualified as DE
import Entity.Error
import Entity.ExternalName qualified as EN
import Entity.Tree
import Scene.Parse.LowType

interpretDeclareTree :: Tree -> App [DE.Decl]
interpretDeclareTree t = do
  (m, declTrees) <- liftEither $ access' "declare" t
  dataSize <- Env.getDataSize m
  liftEither $ mapM (interp dataSize) declTrees

interp :: DS.DataSize -> Tree -> EE DE.Decl
interp size t = do
  (m, ts) <- toNode t
  (extName, rest) <- treeUncons m ts
  (_, atom') <- getSymbol extName
  (argList, body) <- reflArrowArgs' m rest
  argList' <- mapM (interpretLowType size) argList
  body' <- interpretLowType size body
  return $ DE.Decl (EN.ExternalName atom') argList' body'
