module Language.LowComp.Rule.LowComp.EmitValue
  ( emitValue,
    showArgs,
    showFuncArgs,
  )
where

import Data.ByteString.Builder
import Data.Text.Encoding qualified as TE
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.Ident
import Language.Common.Rule.LowType qualified as LT
import Language.Common.Rule.LowType.EmitLowType (emitLowType)
import Language.Common.Rule.PrimNumSize
import Language.LowComp.Rule.LowComp qualified as LC
import Main.Rule.Builder
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
