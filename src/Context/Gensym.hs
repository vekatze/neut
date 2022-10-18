module Context.Gensym
  ( Context (..),
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
import qualified Entity.Comp as C
import Entity.Hint
import Entity.HoleID
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import qualified Entity.PreTerm as PT
import qualified Entity.WeakTerm as WT

class Monad m => Context m where
  newCount :: m Int
  readCount :: m Int
  writeCount :: Int -> m ()

{-# INLINE newText #-}
newText :: Context m => m T.Text
newText = do
  i <- newCount
  return $ ";" <> T.pack (show i)

{-# INLINE newPreAster #-}
newPreAster :: Context m => Hint -> m PT.PreTerm
newPreAster m = do
  i <- HoleID <$> newCount
  return $ m :< PT.Aster i

{-# INLINE newAster #-}
newAster :: Context m => Hint -> [WT.WeakTerm] -> m WT.WeakTerm
newAster m varSeq = do
  i <- HoleID <$> newCount
  return $ m :< WT.Aster i varSeq

{-# INLINE newIdentFromText #-}
newIdentFromText :: Context m => T.Text -> m Ident
newIdentFromText s = do
  i <- newCount
  return $ I (s, i)

{-# INLINE newIdentFromIdent #-}
newIdentFromIdent :: Context m => Ident -> m Ident
newIdentFromIdent x =
  newIdentFromText (Ident.toText x)

{-# INLINE newValueVarLocalWith #-}
newValueVarLocalWith :: Context m => T.Text -> m (Ident, C.Value)
newValueVarLocalWith name = do
  x <- newIdentFromText name
  return (x, C.VarLocal x)

{-# INLINE newTextualIdentFromText #-}
newTextualIdentFromText :: Context m => T.Text -> m Ident
newTextualIdentFromText txt = do
  i <- newCount
  newIdentFromText $ ";" <> txt <> T.pack (show i)
