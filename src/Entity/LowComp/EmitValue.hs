module Entity.LowComp.EmitValue
  ( emitValue,
    showArgs,
    showFuncArgs,
  )
where

import Data.ByteString.Builder
import Entity.Builder
import Entity.DefiniteDescription qualified as DD
import Entity.ExternalName qualified as EN
import Entity.Ident
import Entity.LowComp qualified as LC
import Entity.LowType qualified as LT
import Entity.LowType.EmitLowType (emitLowType)
import Entity.PrimNumSize
import Numeric.Half

emitValue :: LC.Value -> Builder
emitValue lowValue =
  case lowValue of
    LC.VarLocal (I (_, i)) ->
      "%_" <> intDec i
    LC.VarGlobal globalName ->
      "@" <> DD.toBuilder globalName
    LC.VarExternal extName ->
      "@" <> EN.toBuilder extName
    LC.Int i ->
      integerDec i
    LC.Float FloatSize16 x -> do
      let x' = realToFrac x :: Half
      doubleDec (realToFrac x')
    LC.Float FloatSize32 x -> do
      let x' = realToFrac x :: Float
      doubleDec (realToFrac x')
    LC.Float FloatSize64 x -> do
      doubleDec x
    LC.Null ->
      "null"

showArgs :: [(LT.LowType, LC.Value)] -> Builder
showArgs tds =
  showLocals $ map showArg tds

showArg :: (LT.LowType, LC.Value) -> Builder
showArg (t, d) =
  emitLowType t <> " " <> emitValue d

showLocals :: [Builder] -> Builder
showLocals ds =
  "(" <> unwordsC ds <> ")"

showFuncArgs :: [Builder] -> Builder
showFuncArgs ds =
  "(" <> unwordsC (map ("ptr " <>) ds) <> ")"
