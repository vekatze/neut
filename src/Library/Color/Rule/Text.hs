module Library.Color.Rule.Text
  ( Text,
    empty,
    pack,
    pack',
    _unpackWithSGR,
    _unpackWithoutSGR,
  )
where

import Data.String (IsString (..))
import Data.Text qualified as T
import System.Console.ANSI

data Text
  = Nil
  | Cons [SGR] T.Text Text

instance Semigroup Text where
  l1 <> l2 =
    case (l1, l2) of
      (Nil, _) ->
        l2
      (Cons c1 t1 rest1, _) ->
        Cons c1 t1 (rest1 <> l2)

instance Monoid Text where
  mempty =
    Nil

instance IsString Text where
  fromString s =
    pack' (T.pack s)

empty :: Text
empty =
  Nil

pack :: [SGR] -> T.Text -> Text
pack color t =
  Cons color t Nil

pack' :: T.Text -> Text
pack' =
  pack []

_unpackWithSGR :: Text -> T.Text
_unpackWithSGR l =
  case l of
    Nil ->
      ""
    Cons color t rest -> do
      T.pack (setSGRCode color) <> t <> T.pack (setSGRCode [Reset]) <> _unpackWithSGR rest

_unpackWithoutSGR :: Text -> T.Text
_unpackWithoutSGR l = do
  case l of
    Nil ->
      ""
    Cons _ t rest -> do
      t <> _unpackWithoutSGR rest
