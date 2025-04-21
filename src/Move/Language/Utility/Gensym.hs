module Move.Language.Utility.Gensym
  ( newCount,
    newTextForHole,
    newPreHole,
  )
where

import Control.Comonad.Cofree
import Data.IORef (IORef, atomicModifyIORef')
import Data.Text qualified as T
import Rule.Const
import Rule.Hint (Hint)
import Rule.HoleID (HoleID (HoleID))
import Rule.RawTerm qualified as RT

{-# INLINE newCount #-}
newCount :: IORef Int -> IO Int
newCount ref =
  atomicModifyIORef' ref (\x -> (x + 1, x))

{-# INLINE newTextForHole #-}
newTextForHole :: IORef Int -> IO T.Text
newTextForHole ref = do
  i <- newCount ref
  return $ holeVarPrefix <> ";" <> T.pack (show i)

{-# INLINE newPreHole #-}
newPreHole :: IORef Int -> Hint -> IO RT.RawTerm
newPreHole ref m = do
  i <- HoleID <$> newCount ref
  return $ m :< RT.Hole i
