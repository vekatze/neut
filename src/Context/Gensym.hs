module Context.Gensym
  ( Context (..),
    newText,
    newHole,
    newPreHole,
    newIdentFromText,
    newIdentFromIdent,
    newValueVarLocalWith,
    newTextualIdentFromText,
  )
where

import Control.Comonad.Cofree
import Data.Text qualified as T
import Entity.Comp qualified as C
import Entity.Hint
import Entity.HoleID
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.RawTerm qualified as RT
import Entity.WeakTerm qualified as WT

class Monad m => Context m where
  newCount :: m Int
  readCount :: m Int
  writeCount :: Int -> m ()

{-# INLINE newText #-}
newText :: Context m => m T.Text
newText = do
  i <- newCount
  return $ ";" <> T.pack (show i)

{-# INLINE newPreHole #-}
newPreHole :: Context m => Hint -> m RT.RawTerm
newPreHole m = do
  i <- HoleID <$> newCount
  return $ m :< RT.Hole i

{-# INLINE newHole #-}
newHole :: Context m => Hint -> [WT.WeakTerm] -> m WT.WeakTerm
newHole m varSeq = do
  i <- HoleID <$> newCount
  return $ m :< WT.Hole i varSeq

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
