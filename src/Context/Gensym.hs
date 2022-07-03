module Context.Gensym
  ( Axis (..),
    Config (..),
    newText,
    newAster,
    newIdentFromText,
    newIdentFromIdent,
    newValueVarLocalWith,
    newTextualIdentFromText,
  )
where

import Control.Comonad.Cofree
import qualified Data.Text as T
import Entity.Comp
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import Entity.WeakTerm

data Axis = Axis
  { newCount :: IO Int,
    readCount :: IO Int,
    writeCount :: Int -> IO ()
  }

data Config = Config {}

{-# INLINE newText #-}
newText :: Axis -> IO T.Text
newText axis = do
  i <- newCount axis
  return $ ";" <> T.pack (show i)

{-# INLINE newAster #-}
newAster :: Axis -> Hint -> IO WeakTerm
newAster axis m = do
  i <- newCount axis
  return $ m :< WeakTermAster i

{-# INLINE newIdentFromText #-}
newIdentFromText :: Axis -> T.Text -> IO Ident
newIdentFromText axis s = do
  i <- newCount axis
  return $ I (s, i)

{-# INLINE newIdentFromIdent #-}
newIdentFromIdent :: Axis -> Ident -> IO Ident
newIdentFromIdent axis x =
  newIdentFromText axis (Ident.toText x)

{-# INLINE newValueVarLocalWith #-}
newValueVarLocalWith :: Axis -> T.Text -> IO (Ident, Value)
newValueVarLocalWith axis name = do
  x <- newIdentFromText axis name
  return (x, ValueVarLocal x)

{-# INLINE newTextualIdentFromText #-}
newTextualIdentFromText :: Axis -> T.Text -> IO Ident
newTextualIdentFromText axis txt = do
  i <- newCount axis
  newIdentFromText axis $ ";" <> txt <> T.pack (show i)
