module Kernel.Common.GlobalName
  ( GlobalName (..),
    getIsConstLike,
    isNullary,
    disableConstLikeFlag,
    isMetaConstant,
    toMetaFunction,
  )
where

import Data.Text qualified as T
import Language.Common.Arity
import Language.Common.CallSite (IsSourceArg)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.IsConstLike
import Language.Common.IsDestPassing
import Language.Common.PrimOp
import Language.Common.PrimType qualified as PT
import Language.Common.RuleKind (RuleKind)
import Logger.Hint

data GlobalName
  = TopLevelFuncTerm Arity IsConstLike IsDestPassing
  | TopLevelMetaTerm Arity IsConstLike
  | TopLevelFuncType Arity IsConstLike Bool
  | PrimType PT.PrimType
  | PrimOp PrimOp
  | Data Arity [(DD.DefiniteDescription, (Hint, GlobalName))] IsConstLike
  | DataIntro Arity [T.Text] Arity D.Discriminant IsConstLike [IsSourceArg]
  | Rule RuleKind
  | Trope
  | Namespace

getIsConstLike :: GlobalName -> IsConstLike
getIsConstLike gn =
  case gn of
    TopLevelFuncTerm _ isConstLike _ ->
      isConstLike
    TopLevelMetaTerm _ isConstLike ->
      isConstLike
    TopLevelFuncType _ isConstLike _ ->
      isConstLike
    Data _ _ isConstLike ->
      isConstLike
    DataIntro _ _ _ _ isConstLike _ ->
      isConstLike
    Trope ->
      False
    _ ->
      False

isNullary :: GlobalName -> Bool
isNullary gn =
  case gn of
    TopLevelFuncTerm arity _ _ ->
      arity == fromInt 0
    TopLevelMetaTerm arity _ ->
      arity == fromInt 0
    TopLevelFuncType arity _ _ ->
      arity == fromInt 0
    Data arity _ _ ->
      arity == fromInt 0
    DataIntro dataArity consImpParamNames consArity _ _ _ ->
      dataArity == fromInt 0 && null consImpParamNames && consArity == fromInt 0
    Rule {} ->
      False
    Trope ->
      False
    Namespace ->
      False
    PrimType _ ->
      True
    PrimOp _ ->
      False

disableConstLikeFlag :: GlobalName -> GlobalName
disableConstLikeFlag gn =
  case gn of
    TopLevelFuncTerm arity _ isDestPassing ->
      TopLevelFuncTerm arity False isDestPassing
    TopLevelMetaTerm arity _ ->
      TopLevelMetaTerm arity False
    TopLevelFuncType arity _ isMacro ->
      TopLevelFuncType arity False isMacro
    Data arity consInfo _ ->
      Data arity consInfo False
    DataIntro dataArity consImpParamNames consArity discriminant False sourceFlags ->
      DataIntro dataArity consImpParamNames consArity discriminant False sourceFlags
    Trope ->
      Trope
    _ ->
      gn

isMetaConstant :: GlobalName -> Bool
isMetaConstant gn =
  case gn of
    TopLevelMetaTerm _ True ->
      True
    _ ->
      False

toMetaFunction :: GlobalName -> GlobalName
toMetaFunction gn =
  case gn of
    TopLevelMetaTerm arity True ->
      TopLevelMetaTerm arity False
    _ ->
      gn
