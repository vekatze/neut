module Entity.Rune
  ( Rune,
    make,
    asInt,
    asText,
  )
where

import Codec.Binary.UTF8.String
import Data.Binary
import Data.Bits (shiftL, (.|.))
import Data.List qualified as List
import Data.Text qualified as T
import Entity.Text.Util (parseText)
import GHC.Generics (Generic)

data Rune
  = MkRune Char T.Text
  deriving (Eq, Ord, Show, Generic)

instance Binary Rune

make :: T.Text -> Either T.Text Rune
make t = do
  case parseText t of
    Left reason ->
      Left $ "Could not interpret the following as a rune: " <> t <> "\nReason: " <> reason
    Right str' ->
      case T.uncons str' of
        Just (c, "") -> do
          return $ MkRune c t
        _ -> do
          Left "The content of a rune literal must be of length 1"

asInt :: Rune -> Integer
asInt (MkRune r _) = do
  let r' = encodeChar r
  List.foldl' (\acc byte -> (acc `shiftL` 8) .|. fromIntegral byte) 0 r'

asText :: Rune -> T.Text
asText (MkRune _ orig) =
  orig
