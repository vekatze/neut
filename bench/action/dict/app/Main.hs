module Main (main) where

import Data.Map.Strict qualified as M
import System.Environment (getArgs)
import System.Random
import Text.Read

makeBigDict :: Int -> M.Map Int Int -> IO (M.Map Int Int)
makeBigDict len acc =
  if len == 0
    then return acc
    else do
      k <- randomRIO (0, 1000000)
      v <- randomRIO (0, 1000000)
      makeBigDict (len - 1) $ M.insert k v acc

randomSum :: Int -> Int -> M.Map Int Int -> IO Int
randomSum count acc bigDict = do
  if count <= 0
    then return acc
    else do
      k <- randomRIO (0, 1000000)
      case M.lookup k bigDict of
        Nothing ->
          randomSum (count - 1) acc bigDict
        Just val ->
          randomSum (count - 1) (acc + val) bigDict

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sizeStr]
      | Just size <- readMaybe sizeStr -> do
          bigDict <- makeBigDict size M.empty
          val <- randomSum size 0 bigDict
          print val
    _ ->
      putStrLn "usage: dict-hs-exe SIZE"
