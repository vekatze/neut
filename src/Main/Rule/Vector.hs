module Main.Rule.Vector (swap) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Error.Rule.Error
import Language.Common.Rule.Error
import Language.Common.Rule.Hint

swap :: Hint -> Int -> V.Vector a -> Either Error (V.Vector a)
swap m i xs = do
  let len = length xs
  if not (0 <= i && i < len)
    then Left $ newCritical m $ T.pack $ "the index " ++ show i ++ " exceeds the list size " ++ show len ++ "."
    else return $ V.update xs $ V.fromList [(0, xs V.! i), (i, xs V.! 0)]
