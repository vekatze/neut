module Kernel.Emit.LowValue
  ( emitValue,
    emitIdentAsVar,
    emitIdentAsLabel,
    emitIdentAsLabelVar,
    showArgs,
    showArgsWithSRet,
    showFuncArgs,
    showFuncArgsWithSRet,
  )
where

import Data.ByteString.Builder
import Data.Text.Encoding qualified as TE
import Kernel.Emit.Builder
import Kernel.Emit.LowType (emitLowType)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.Ident
import Language.Common.LowType qualified as LT
import Language.Common.PrimNumSize
import Language.LowComp.LowComp qualified as LC
import Numeric.Half

emitValue :: LC.Value -> Builder
emitValue lowValue =
  case lowValue of
    LC.VarLocal x ->
      "%" <> emitIdentAsVar x
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
    LC.Address a ->
      if a == 0
        then "null"
        else "inttoptr (i64 " <> integerDec a <> " to ptr)"
    LC.Null ->
      "null"

emitIdentAsVar :: Ident -> Builder
emitIdentAsVar (I (_, i)) =
  "v" <> intDec i

emitIdentAsLabel :: Ident -> Builder
emitIdentAsLabel (I (_, i)) =
  "L" <> intDec i

emitIdentAsLabelVar :: Ident -> Builder
emitIdentAsLabelVar x =
  "%" <> emitIdentAsLabel x

showArgs :: [(LT.LowType, LC.Value)] -> Builder
showArgs tds =
  showLocals $ map showArg tds

showArgsWithSRet :: [(LT.LowType, LC.Value)] -> Builder
showArgsWithSRet tds =
  showLocals $ zipWith (curry showArgWithSRet) [0 ..] tds

showArg :: (LT.LowType, LC.Value) -> Builder
showArg (t, d) =
  emitLowType t <> " " <> emitValue d

showArgWithSRet :: (Int, (LT.LowType, LC.Value)) -> Builder
showArgWithSRet (i, (t, d))
  | i == 0 && t == LT.Pointer =
      emitLowType t <> " sret(ptr) " <> emitValue d
  | otherwise =
      showArg (t, d)

showLocals :: [Builder] -> Builder
showLocals ds =
  "(" <> unwordsC ds <> ")"

showFuncArgs :: [Builder] -> Builder
showFuncArgs ds =
  "(" <> unwordsC (map ("ptr " <>) ds) <> ")"

showFuncArgsWithSRet :: [Builder] -> Builder
showFuncArgsWithSRet ds =
  showLocals $ zipWith (curry emitFuncArgWithSRet) [0 ..] ds

emitFuncArgWithSRet :: (Int, Builder) -> Builder
emitFuncArgWithSRet (i, d)
  | i == 0 =
      "ptr sret(ptr) " <> d
  | otherwise =
      "ptr " <> d
