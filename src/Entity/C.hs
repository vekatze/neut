module Entity.C where

import Data.Text qualified as T

-- list of comments
type C = [T.Text]

type ArgList a =
  ([(C, a)], C)

distillArgList :: ArgList a -> [a]
distillArgList (args, _) =
  map snd args
