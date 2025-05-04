module Main.Move.Context.Gensym
  ( newTextForHole,
    newIdentFromText,
    newText,
    newIdentFromIdent,
    newTextFromText,
    newIdentForHole,
    newValueVarLocalWith,
  )
where

import Data.Text qualified as T
import Gensym.Move.Gensym
import Gensym.Rule.Handle
import Language.Common.Rule.Const
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify qualified as Ident
import Language.Comp.Rule.Comp qualified as C

{-# INLINE newTextForHole #-}
newTextForHole :: Handle -> IO T.Text
newTextForHole h = do
  i <- newCount h
  return $ holeVarPrefix <> ";" <> T.pack (show i)

{-# INLINE newIdentFromText #-}
newIdentFromText :: Handle -> T.Text -> IO Ident
newIdentFromText h s = do
  i <- newCount h
  return $ I (s, i)

{-# INLINE newText #-}
newText :: Handle -> IO T.Text
newText h = do
  i <- newCount h
  return $ ";" <> T.pack (show i)

{-# INLINE newIdentFromIdent #-}
newIdentFromIdent :: Handle -> Ident -> IO Ident
newIdentFromIdent h x =
  newIdentFromText h (Ident.toText x)

{-# INLINE newTextFromText #-}
newTextFromText :: Handle -> T.Text -> IO T.Text
newTextFromText h base = do
  i <- newCount h
  return $ ";" <> base <> T.pack (show i)

{-# INLINE newIdentForHole #-}
newIdentForHole :: Handle -> IO Ident
newIdentForHole h = do
  text <- newTextForHole h
  i <- newCount h
  return $ I (text, i)

{-# INLINE newValueVarLocalWith #-}
newValueVarLocalWith :: Handle -> T.Text -> IO (Ident, C.Value)
newValueVarLocalWith h name = do
  x <- newIdentFromText h name
  return (x, C.VarLocal x)
