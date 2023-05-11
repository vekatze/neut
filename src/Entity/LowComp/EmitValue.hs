module Entity.LowComp.EmitValue
  ( emitValue,
    showArgs,
    showLocals,
  )
where

import Data.ByteString.Builder
import Entity.Builder
import Entity.DefiniteDescription qualified as DD
import Entity.ExternalName qualified as EN
import Entity.Ident
import Entity.LowComp qualified as LC
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
      "0x" <> doubleHexFixed (realToFrac x')
    LC.Float FloatSize32 x -> do
      let x' = realToFrac x :: Float
      "0x" <> doubleHexFixed (realToFrac x')
    LC.Float FloatSize64 x -> do
      let x' = realToFrac x :: Double
      "0x" <> doubleHexFixed (realToFrac x')
    LC.Null ->
      "null"

showArgs :: [LC.Value] -> Builder
showArgs ds =
  showLocals $ map emitValue ds

showLocal :: Builder -> Builder
showLocal x =
  "ptr " <> x

showLocals :: [Builder] -> Builder
showLocals ds =
  "(" <> unwordsC (map showLocal ds) <> ")"
