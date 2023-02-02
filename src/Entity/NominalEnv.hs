module Entity.NominalEnv where

import Control.Comonad.Cofree
import qualified Data.Text as T
import Entity.Hint
import Entity.Ident
import qualified Entity.WeakTerm as WT

type NominalEnv = [(T.Text, (Hint, Ident))]

asHoleArgs :: NominalEnv -> [WT.WeakTerm]
asHoleArgs = do
  map (\(_, (mx, x)) -> mx :< WT.Var x)

empty :: NominalEnv
empty = []
