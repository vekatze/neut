module Move.Context.Gensym
  ( newCount,
    newText,
    newTextFromText,
    newTextForHole,
    newIdentForHole,
    newHole,
    newPreHole,
    newIdentFromText,
    newIdentFromIdent,
    newValueVarLocalWith,
    newTextualIdentFromText,
    getCount,
    setCount,
  )
where

import Move.Context.App
import Move.Context.App.Internal
import Control.Comonad.Cofree
import Control.Monad (void)
import Control.Monad.Reader
import Data.IORef
import Data.Text qualified as T
import Rule.Comp qualified as C
import Rule.Const
import Rule.Hint
import Rule.HoleID
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.RawTerm qualified as RT
import Rule.WeakTerm qualified as WT

newCount :: App Int
newCount =
  asks counter >>= \ref -> liftIO $ atomicModifyIORef' ref (\x -> (x + 1, x))

{-# INLINE newText #-}
newText :: App T.Text
newText = do
  i <- newCount
  return $ ";" <> T.pack (show i)

{-# INLINE newTextFromText #-}
newTextFromText :: T.Text -> App T.Text
newTextFromText base = do
  i <- newCount
  return $ ";" <> base <> T.pack (show i)

{-# INLINE newTextForHole #-}
newTextForHole :: App T.Text
newTextForHole = do
  i <- newCount
  return $ holeVarPrefix <> ";" <> T.pack (show i)

{-# INLINE newIdentForHole #-}
newIdentForHole :: App Ident
newIdentForHole = do
  text <- newTextForHole
  i <- newCount
  return $ I (text, i)

{-# INLINE newPreHole #-}
newPreHole :: Hint -> App RT.RawTerm
newPreHole m = do
  i <- HoleID <$> newCount
  return $ m :< RT.Hole i

{-# INLINE newHole #-}
newHole :: Hint -> [WT.WeakTerm] -> App WT.WeakTerm
newHole m varSeq = do
  i <- HoleID <$> newCount
  return $ m :< WT.Hole i varSeq

{-# INLINE newIdentFromText #-}
newIdentFromText :: T.Text -> App Ident
newIdentFromText s = do
  i <- newCount
  return $ I (s, i)

{-# INLINE newIdentFromIdent #-}
newIdentFromIdent :: Ident -> App Ident
newIdentFromIdent x =
  newIdentFromText (Ident.toText x)

{-# INLINE newValueVarLocalWith #-}
newValueVarLocalWith :: T.Text -> App (Ident, C.Value)
newValueVarLocalWith name = do
  x <- newIdentFromText name
  return (x, C.VarLocal x)

{-# INLINE newTextualIdentFromText #-}
newTextualIdentFromText :: T.Text -> App Ident
newTextualIdentFromText txt = do
  i <- newCount
  newIdentFromText $ ";" <> txt <> T.pack (show i)

getCount :: App Int
getCount =
  asks counter >>= \ref -> liftIO $ readIORef ref

setCount :: Int -> App ()
setCount countSnapshot = do
  asks counter >>= \ref -> liftIO $ void $ atomicModifyIORef' ref (\x -> (max x countSnapshot, ()))
