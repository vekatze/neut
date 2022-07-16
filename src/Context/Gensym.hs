module Context.Gensym
  ( Context (..),
    Config (..),
    newText,
    newAster,
    newPreAster,
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
import Entity.HoleID
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import qualified Entity.PreTerm as PT
import Entity.WeakTerm

data Context = Context
  { newCount :: IO Int,
    readCount :: IO Int,
    writeCount :: Int -> IO ()
  }

data Config = Config {}

{-# INLINE newText #-}
newText :: Context -> IO T.Text
newText ctx = do
  i <- newCount ctx
  return $ ";" <> T.pack (show i)

{-# INLINE newPreAster #-}
newPreAster :: Context -> Hint -> IO PT.PreTerm
newPreAster ctx m = do
  i <- HoleID <$> newCount ctx
  return $ m :< PT.Aster i

{-# INLINE newAster #-}
newAster :: Context -> Hint -> IO WeakTerm
newAster ctx m = do
  i <- HoleID <$> newCount ctx
  return $ m :< WeakTermAster i

{-# INLINE newIdentFromText #-}
newIdentFromText :: Context -> T.Text -> IO Ident
newIdentFromText ctx s = do
  i <- newCount ctx
  return $ I (s, i)

{-# INLINE newIdentFromIdent #-}
newIdentFromIdent :: Context -> Ident -> IO Ident
newIdentFromIdent ctx x =
  newIdentFromText ctx (Ident.toText x)

{-# INLINE newValueVarLocalWith #-}
newValueVarLocalWith :: Context -> T.Text -> IO (Ident, Value)
newValueVarLocalWith ctx name = do
  x <- newIdentFromText ctx name
  return (x, ValueVarLocal x)

{-# INLINE newTextualIdentFromText #-}
newTextualIdentFromText :: Context -> T.Text -> IO Ident
newTextualIdentFromText ctx txt = do
  i <- newCount ctx
  newIdentFromText ctx $ ";" <> txt <> T.pack (show i)
