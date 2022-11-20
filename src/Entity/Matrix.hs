module Entity.Matrix where

import qualified Context.Throw as Throw
import qualified Data.Vector as V
import Entity.Hint

data Matrix a = M
  { rows :: Int,
    columns :: Int,
    elements :: V.Vector a
  }

-- 3, 2 [x, y, z, a, b, c] →
-- [[x, y],
--  [z, a],
--  [b, c]]
new :: Throw.Context m => Hint -> Int -> Int -> [a] -> m (Matrix a)
new m r c es =
  if length es /= r * c
    then Throw.raiseError m "invalid dimension"
    else
      return
        M
          { rows = r,
            columns = c,
            elements = V.fromList es
          }

-- insert :: a -> Matrix a -> Matrix a
-- insert v mat = mat { }
