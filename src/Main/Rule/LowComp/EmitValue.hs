module Main.Rule.LowComp.EmitValue
  ( emitValue,
    showArgs,
    showFuncArgs,
  )
where

import Data.ByteString.Builder
import Data.Text.Encoding qualified as TE
import Main.Rule.Builder
import Main.Rule.DefiniteDescription qualified as DD
import Main.Rule.ExternalName qualified as EN
import Main.Rule.Ident
import Main.Rule.LowComp qualified as LC
import Main.Rule.LowType qualified as LT
import Main.Rule.LowType.EmitLowType (emitLowType)
import Main.Rule.PrimNumSize
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
    LC.VarTextName textName ->
      "@" <> TE.encodeUtf8Builder ("\"" <> textName <> "\"")
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
