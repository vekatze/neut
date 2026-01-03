module Kernel.Elaborate.Stuck
  ( EvalBase (..),
    EvalCtx,
    EvalCtxF (..),
    Stuck,
    asStuckedType,
    resume,
    asPairList,
  )
where

import App.App (App)
import Control.Comonad.Cofree
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IntMap qualified as IntMap
import Kernel.Elaborate.Constraint qualified as C
import Kernel.Elaborate.Internal.Handle.WeakTypeDef (TypeDef (..))
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.HoleID qualified as HID
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.WeakTerm.Subst (substTypeWith)
import Language.WeakTerm.WeakPrimValue qualified as WPV
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint

data EvalBase
  = VarLocal Ident
  | VarGlobal DD.DefiniteDescription
  | Hole HID.HoleID [WT.WeakType]
  | Prim (WPV.WeakPrimValue WT.WeakType)

type EvalCtx = Cofree EvalCtxF Hint

data EvalCtxF a
  = Base
  | App a [WT.WeakType]

type Stuck = (EvalBase, EvalCtx)

asStuckedType :: WT.WeakType -> Maybe Stuck
asStuckedType term =
  case term of
    m :< WT.TVar x ->
      Just (VarLocal x, m :< Base)
    m :< WT.TVarGlobal _ g ->
      Just (VarGlobal g, m :< Base)
    m :< WT.TypeHole h es ->
      Just (Hole h es, m :< Base)
    m :< WT.TyApp e args -> do
      (base, ctx) <- asStuckedType e
      return (base, m :< App ctx args)
    _ ->
      Nothing

resume :: TypeDef -> EvalCtx -> IO (Maybe WT.WeakType)
resume typeDef ctx = do
  let TypeDef {typeDefBinders, typeDefBody} = typeDef
  case (typeDefBinders, ctx) of
    ([], _ :< Base) ->
      return $ Just typeDefBody
    (params, _ :< App ctx' args)
      | length params == length args -> do
          mInner <- resume (TypeDef [] typeDefBody) ctx'
          case mInner of
            Just inner -> do
              let sub = IntMap.fromList $ zipWith (\(_, x, _) t -> (Ident.toInt x, Right t)) params args
              Just <$> substTypeWith sub inner
            Nothing ->
              return Nothing
    _ ->
      return Nothing

asPairList :: EvalCtx -> EvalCtx -> Maybe [C.Constraint]
asPairList ctx1 ctx2 =
  case (ctx1, ctx2) of
    (_ :< Base, _ :< Base) ->
      Just []
    (_ :< App ctx1' args1, _ :< App ctx2' args2)
      | length args1 == length args2 -> do
          pairList <- asPairList ctx1' ctx2'
          return $ zipWith C.Eq args1 args2 ++ pairList
    _ ->
      Nothing
