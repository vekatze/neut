{-# LANGUAGE OverloadedStrings #-}

module Language.LowComp.Render (renderDef) where

import Data.Text (Text)
import Data.Text qualified as T
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident.Reify (toText')
import Language.Common.LowType
import Language.Common.PrimType.ToText qualified as PT
import Language.LowComp.LowComp qualified as LC

renderDef :: LC.Def -> Text
renderDef (name, LC.DefContent {codType = codType, args = args, body = body}) =
  "define "
    <> DD.localLocator name
    <> "("
    <> T.intercalate ", " (map toText' args)
    <> ") -> "
    <> renderLowType codType
    <> " {\n"
    <> renderComp 1 body
    <> "}"

renderComp :: Int -> LC.Comp -> Text
renderComp level comp =
  case comp of
    LC.Return value ->
      line level ("return " <> renderValue value)
    LC.ReturnVoid ->
      line level "return-void"
    LC.Let x op cont ->
      line level ("let " <> toText' x <> " = " <> renderOp op) <> renderComp level cont
    LC.Cont op cont ->
      line level (renderOp op) <> renderComp level cont
    LC.Switch value lowType defaultBranch caseList _ cont ->
      line level ("switch " <> renderValue value <> ": " <> renderLowType lowType <> " {")
        <> T.concat (map (renderCase level) caseList)
        <> line level "| default =>"
        <> renderComp (level + 1) defaultBranch
        <> line level "}"
        <> renderComp level cont
    LC.TailCall lowType value args ->
      line level ("tail-call " <> renderLowType lowType <> " " <> renderValue value <> renderArgs args)
    LC.Unreachable ->
      line level "⊥"
    LC.Phi values ->
      line level ("phi [" <> T.intercalate ", " (map renderValue values) <> "]")

renderCase :: Int -> (Integer, LC.Comp) -> Text
renderCase level (key, branch) =
  line level ("| <Int " <> tshow key <> "> =>") <> renderComp (level + 1) branch

renderOp :: LC.Op -> Text
renderOp op =
  case op of
    LC.Call codType value args ->
      "call " <> renderLowType codType <> " " <> renderValue value <> renderArgs args
    LC.MagicCall codType value args ->
      "magic-call " <> renderLowType codType <> " " <> renderValue value <> renderArgs args
    LC.GetElementPtr (base, baseType) indices ->
      "get-element-ptr " <> renderValue base <> ": " <> renderLowType baseType <> renderPairs indices
    LC.Bitcast value fromType toType ->
      "bitcast " <> renderValue value <> " from " <> renderLowType fromType <> " to " <> renderLowType toType
    LC.IntToPointer value fromType ->
      "int-to-pointer " <> renderValue value <> " from " <> renderLowType fromType
    LC.PointerToInt value toType ->
      "pointer-to-int " <> renderValue value <> " to " <> renderLowType toType
    LC.Load value lowType ->
      "load " <> renderLowType lowType <> " " <> renderValue value
    LC.Store lowType value pointer ->
      "store " <> renderLowType lowType <> " " <> renderValue value <> " " <> renderValue pointer
    LC.StackAlloc info ->
      "stack-alloc " <> renderLowType (LC.stackElemType info) <> " " <> renderLowType (LC.stackIndexType info)
    LC.StackLifetimeStart slot ->
      "stack-lifetime-start " <> tshow slot
    LC.StackLifetimeEnd slot ->
      "stack-lifetime-end " <> tshow slot
    LC.Calloc count size ->
      "calloc " <> renderValue count <> " " <> renderValue size
    LC.Alloc size ident ->
      "alloc " <> either tshow renderValue size <> " (#" <> tshow ident <> ")"
    LC.Realloc pointer size ->
      "realloc " <> renderValue pointer <> " " <> renderValue size
    LC.Free value _ ident ->
      "free " <> renderValue value <> " (#" <> tshow ident <> ")"
    LC.PrimOp primitive values ->
      "prim-op " <> tshow primitive <> " " <> T.intercalate ", " (map renderValue values)

renderArgs :: [(LowType, LC.Value)] -> Text
renderArgs args =
  "(" <> T.intercalate ", " (map renderTypedValue args) <> ")"

renderPairs :: [(LC.Value, LowType)] -> Text
renderPairs pairs =
  " [" <> T.intercalate ", " (map renderValueType pairs) <> "]"

renderTypedValue :: (LowType, LC.Value) -> Text
renderTypedValue (lowType, value) =
  renderLowType lowType <> " " <> renderValue value

renderValueType :: (LC.Value, LowType) -> Text
renderValueType (value, lowType) =
  renderValue value <> ": " <> renderLowType lowType

renderValue :: LC.Value -> Text
renderValue =
  T.pack . show

renderLowType :: LowType -> Text
renderLowType lowType =
  case lowType of
    PrimNum primType ->
      PT.toText primType
    Pointer ->
      "ptr"
    Array size elementType ->
      "[" <> tshow size <> " x " <> renderLowType elementType <> "]"
    Struct types ->
      "{" <> T.intercalate ", " (map renderLowType types) <> "}"
    Function dom cod ->
      "(" <> T.intercalate ", " (map renderLowType dom) <> ") -> " <> renderLowType cod
    Void ->
      "void"
    VarArgs ->
      "..."

line :: Int -> Text -> Text
line level text = T.replicate (level * 2) " " <> text <> "\n"

tshow :: (Show a) => a -> Text
tshow =
  T.pack . show
