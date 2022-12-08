module Case.Main.Global
  ( registerTopLevelFunc,
    registerData,
    registerDataIntro,
    lookup,
    initialize,
    Context,
  )
where

import qualified Context.Env as Env
import qualified Context.Throw as Throw
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import qualified Entity.Arity as A
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import qualified Entity.GlobalName as GN
import qualified Entity.Hint as Hint
import qualified Entity.PrimOp.FromText as PrimOp
import qualified Entity.PrimType.FromText as PT
import Prelude hiding (lookup)

type NameMap = Map.HashMap DD.DefiniteDescription GN.GlobalName

class
  ( Throw.Context m,
    Env.Context m,
    MonadIO m
  ) =>
  Context m

initialize :: Context m => m ()
initialize = do
  Env.setNameMap Map.empty

-- forM_ defaultDataEnv $ \(typeName, enumItemList) ->
--   forM_ (createDataMap typeName enumItemList) $
--     uncurry Env.insertToNameMap

registerTopLevelFunc ::
  Context m =>
  Hint.Hint ->
  DD.DefiniteDescription ->
  A.Arity ->
  m ()
registerTopLevelFunc m topLevelName arity = do
  topNameMap <- Env.getNameMap
  ensureFreshness m topNameMap topLevelName
  Env.insertToNameMap topLevelName $ GN.TopLevelFunc arity

registerData ::
  Context m =>
  Hint.Hint ->
  DD.DefiniteDescription ->
  A.Arity ->
  [DD.DefiniteDescription] ->
  m ()
registerData m dataName arity consList = do
  topNameMap <- Env.getNameMap
  ensureFreshness m topNameMap dataName
  Env.insertToNameMap dataName $ GN.Data arity consList

registerDataIntro ::
  Context m =>
  Hint.Hint ->
  DD.DefiniteDescription ->
  A.Arity ->
  A.Arity ->
  D.Discriminant ->
  m ()
registerDataIntro m consName dataArity consArity disc = do
  topNameMap <- Env.getNameMap
  ensureFreshness m topNameMap consName
  Env.insertToNameMap consName $ GN.DataIntro dataArity consArity disc

ensureFreshness :: Context m => Hint.Hint -> NameMap -> DD.DefiniteDescription -> m ()
ensureFreshness m topNameMap name = do
  when (Map.member name topNameMap) $
    Throw.raiseError m $
      "`" <> DD.reify name <> "` is already defined"

lookup :: Context m => DD.DefiniteDescription -> m (Maybe GN.GlobalName)
lookup name = do
  nameMap <- Env.getNameMap
  case Map.lookup name nameMap of
    Just kind ->
      return $ Just kind
    Nothing
      | Just primType <- PT.fromDefiniteDescription name ->
          return $ Just $ GN.PrimType primType
      | Just primOp <- PrimOp.fromDefiniteDescription name ->
          return $ Just $ GN.PrimOp primOp
      | otherwise -> do
          return Nothing

-- createDataMap :: DD.DefiniteDescription -> [DI.DataValue] -> [(DD.DefiniteDescription, GN.GlobalName)]
-- createDataMap dataName consInfoList = do
--   let (consNameList, discriminants) = unzip consInfoList
--   let zero = A.fromInt 0
--   (dataName, GN.Data zero consNameList) : zip consNameList (map (GN.DataIntro zero zero) discriminants)

-- defaultDataEnv :: [(DD.DefiniteDescription, [DI.DataValue])]
-- defaultDataEnv =
--   [ (DI.constBottom, []),
--     (DI.constTop, [(DI.constTopUnit, D.zero)]),
--     (DI.constBool, [(DI.constBoolFalse, D.zero), (DI.constBoolTrue, D.increment D.zero)])
--   ]
