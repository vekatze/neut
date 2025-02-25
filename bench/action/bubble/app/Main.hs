import System.Environment (getArgs)
import System.Random
import Text.Read

data List = MyNil | MyCons !Int !List

{-# INLINE swapGT #-}
swapGT :: Bool -> Int -> Int -> List -> List
swapGT False v x xs = MyCons v (MyCons x xs)
swapGT True v x xs = MyCons x (insert v xs)

insert :: Int -> List -> List
insert v MyNil = MyCons v MyNil
insert v (MyCons x xs) = swapGT (v > x) v x xs

sort :: List -> List -> List
sort MyNil acc = acc
sort (MyCons x xs) acc = sort xs (insert x acc)

randList :: Int -> List -> IO List
randList len acc = do
  if len == 0
    then return acc
    else do
      v <- randomRIO (0, 10000)
      randList (len - 1) (MyCons v acc)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sizeStr]
      | Just size <- readMaybe sizeStr -> do
          someList <- randList size MyNil
          let !_ = sort someList MyNil
          return ()
    _ ->
      putStrLn "usage: bubble-hs-exe SIZE"
