module Entity.Log
  ( Log (..),
    pack,
    pack',
    unpackWithSGR,
    unpackWithoutSGR,
  )
where

import Data.Text qualified as T
import System.Console.ANSI

data Log
  = Nil
  | Cons [SGR] T.Text Log

instance Semigroup Log where
  l1 <> l2 =
    case (l1, l2) of
      (Nil, _) ->
        l2
      (Cons c1 t1 rest1, _) ->
        Cons c1 t1 (rest1 <> l2)

instance Monoid Log where
  mempty =
    Nil

pack :: [SGR] -> T.Text -> Log
pack color t =
  Cons color t Nil

pack' :: T.Text -> Log
pack' t =
  Cons [] t Nil

unpackWithSGR :: Log -> T.Text
unpackWithSGR l =
  case l of
    Nil ->
      ""
    Cons color t rest -> do
      T.pack (setSGRCode color) <> t <> T.pack (setSGRCode [Reset]) <> unpackWithSGR rest

unpackWithoutSGR :: Log -> T.Text
unpackWithoutSGR l = do
  case l of
    Nil ->
      ""
    Cons _ t rest -> do
      t <> unpackWithoutSGR rest
