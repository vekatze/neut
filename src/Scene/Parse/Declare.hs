module Scene.Parse.Declare (interpretDeclareTree) where

import Context.App
import Context.Env qualified as Env
import Context.Throw (liftEither)
import Control.Comonad.Cofree
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

interp :: DS.DataSize -> Tree -> Either Error DE.Decl
interp size t =
  case t of
    _ :< Node [extName, extDom, extCod] -> do
      (_, atom') <- getSymbol extName
      (_, extDom') <- toList1 extDom
      extDom'' <- mapM (interpretLowType size) extDom'
      extCod' <- interpretLowType size extCod
      return $ DE.Decl (EN.ExternalName atom') extDom'' extCod'
    m :< _ ->
      Left $ newError m "decl-interp"
