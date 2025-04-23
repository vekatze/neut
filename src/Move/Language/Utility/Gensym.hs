module Move.Language.Utility.Gensym
  ( Handle,
    new,
    newCount,
    newTextForHole,
    newPreHole,
    newHole,
  )
where

import Control.Comonad.Cofree
import Control.Monad.Reader (asks)
import Data.IORef (IORef, atomicModifyIORef')
import Data.Text qualified as T
import Move.Context.App (App)
import Move.Context.App.Internal qualified as App
import Rule.Const
import Rule.Hint (Hint)
import Rule.HoleID (HoleID (HoleID))
import Rule.RawTerm qualified as RT
import Rule.WeakTerm qualified as WT

newtype Handle
  = Handle
  { counterRef :: IORef Int
  }

new :: App Handle
new = do
  counterRef <- asks App.counter
  return $ Handle {..}

{-# INLINE newCount #-}
newCount :: Handle -> IO Int
newCount h =
  atomicModifyIORef' (counterRef h) (\x -> (x + 1, x))

{-# INLINE newTextForHole #-}
newTextForHole :: Handle -> IO T.Text
newTextForHole h = do
  i <- newCount h
  return $ holeVarPrefix <> ";" <> T.pack (show i)

{-# INLINE newPreHole #-}
newPreHole :: Handle -> Hint -> IO RT.RawTerm
newPreHole h m = do
  i <- HoleID <$> newCount h
  return $ m :< RT.Hole i

{-# INLINE newHole #-}
newHole :: Handle -> Hint -> [WT.WeakTerm] -> IO WT.WeakTerm
newHole h m varSeq = do
  i <- HoleID <$> newCount h
  return $ m :< WT.Hole i varSeq
