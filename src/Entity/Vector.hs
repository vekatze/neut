module Entity.Vector where

import Context.Throw qualified as Throw
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.Hint

swap :: Throw.Context m => Hint -> Int -> V.Vector a -> m (V.Vector a)
swap m i xs = do
  let len = length xs
  if not (0 <= i && i < len)
    then Throw.raiseCritical m $ T.pack $ "the index " ++ show i ++ " exceeds the list size " ++ show len ++ "."
    else return $ V.update xs $ V.fromList [(0, xs V.! i), (i, xs V.! 0)]
