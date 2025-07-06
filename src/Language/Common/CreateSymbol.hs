module Language.Common.CreateSymbol
  ( newTextForHole,
    newIdentFromText,
    newText,
    newIdentFromIdent,
    newTextFromText,
    newIdentForHole,
  )
where

import Data.Text qualified as T
import Gensym.Gensym
import Gensym.Handle
import Language.Common.Const
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident

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
  newTextForHole h

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
