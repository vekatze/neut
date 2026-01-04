module Kernel.Elaborate.Stuck
  ( EvalBase (..),
    EvalCtx,
    EvalCtxF (..),
    Stuck,
    asStuckedType,
    resume,
    asPairList,
    stuckToText,
    mStuckToText,
  )
where

import Control.Comonad.Cofree
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Kernel.Elaborate.Constraint qualified as C
import Kernel.Elaborate.Internal.Handle.WeakTypeDef (TypeDef (..))
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.HoleID qualified as HID
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.WeakTerm.Subst (SubstEntry (..))
import Language.WeakTerm.Subst qualified as Subst
import Language.WeakTerm.ToText qualified as ToText
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

resume :: Subst.Handle -> TypeDef -> EvalCtx -> IO (Maybe WT.WeakType)
resume substHandle typeDef ctx = do
  let TypeDef {typeDefBinders, typeDefBody} = typeDef
  case (typeDefBinders, ctx) of
    ([], _ :< Base) ->
      return $ Just typeDefBody
    (params, _ :< App ctx' args)
      | length params == length args -> do
          mInner <- resume substHandle (TypeDef [] typeDefBody) ctx'
          case mInner of
            Just inner -> do
              let sub = IntMap.fromList $ zipWith (\(_, x, _) t -> (Ident.toInt x, Type t)) params args
              Just <$> Subst.substType substHandle sub inner
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

mStuckToText :: Maybe Stuck -> T.Text
mStuckToText mStuck =
  case mStuck of
    Nothing ->
      "Nothing"
    Just stuck ->
      "Just (" <> stuckToText stuck <> ")"

stuckToText :: Stuck -> T.Text
stuckToText (base, ctx) =
  "(" <> evalBaseToText base <> ", " <> evalCtxToText ctx <> ")"

evalBaseToText :: EvalBase -> T.Text
evalBaseToText base =
  case base of
    VarLocal x ->
      Ident.toText x
    VarGlobal dd ->
      DD.localLocator dd
    Hole holeId args ->
      "?" <> T.pack (show (HID.reify holeId)) <> "(" <> T.intercalate ", " (map ToText.toTextType args) <> ")"
    Prim primValue ->
      showPrimValue primValue

evalCtxToText :: EvalCtx -> T.Text
evalCtxToText (_ :< ctx) =
  case ctx of
    Base ->
      "*"
    App ctx' args ->
      "App(" <> evalCtxToText ctx' <> ", [" <> T.intercalate ", " (map ToText.toTextType args) <> "])"

-- "App[" <> evalCtxToText ctx' <> "(" <> T.intercalate ", " (map ToText.toTextType args) <> ")]"

showPrimValue :: WPV.WeakPrimValue WT.WeakType -> T.Text
showPrimValue (WPV.Int _ v) =
  T.pack (show v)
showPrimValue (WPV.Float _ v) =
  T.pack (show v)
showPrimValue (WPV.Op op) =
  T.pack (show op)
showPrimValue (WPV.StaticText _ text) =
  T.pack (show text)
showPrimValue (WPV.Rune r) =
  T.pack (show r)
