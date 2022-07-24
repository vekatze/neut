module Entity.Global where

import Data.IORef
import qualified Data.Set as S
import System.IO.Unsafe

--
-- global variables
--

{-# NOINLINE nopFreeSetRef #-}
nopFreeSetRef :: IORef (S.Set Int)
nopFreeSetRef =
  unsafePerformIO (newIORef S.empty)

-- for debug
p :: String -> IO ()
p =
  putStrLn

p' :: (Show a) => a -> IO ()
p' =
  print
