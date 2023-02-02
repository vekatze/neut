module Entity.Vector where

import qualified Context.Throw as Throw
import qualified Data.Text as T
import qualified Data.Vector as V
import Entity.Hint

swap :: Throw.Context m => Hint -> Int -> V.Vector a -> m (V.Vector a)
swap m i xs = do
  let len = length xs
  if not (0 <= i && i < len)
    then Throw.raiseCritical m $ T.pack $ "the index " ++ show i ++ " exceeds the list size " ++ show len ++ "."
    else return $ V.update xs $ V.fromList [(0, xs V.! i), (i, xs V.! 0)]
