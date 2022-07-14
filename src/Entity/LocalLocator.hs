module Entity.LocalLocator
  ( LocalLocator (..),
    new,
    reify,
    reflect,
    extend,
  )
where

import qualified Context.Throw as Throw
import Data.Binary
import Data.Hashable
import qualified Data.Text as T
import qualified Entity.BaseName as BN
import Entity.Const
import qualified Entity.Hint as H
import qualified Entity.Section as S
import GHC.Generics

data LocalLocator = MakeLocalLocator
  { sectionStack :: [S.Section],
    baseName :: BN.BaseName
  }
  deriving (Generic, Show)

instance Binary LocalLocator

instance Hashable LocalLocator

reify :: LocalLocator -> T.Text
reify ll =
  reify' (sectionStack ll) (BN.reify $ baseName ll)

reify' :: [S.Section] -> T.Text -> T.Text
reify' ss acc =
  case ss of
    [] ->
      acc
    S.Section s : rest ->
      reify' rest $ BN.reify s <> nsSep <> acc

reflect :: Throw.Context -> H.Hint -> T.Text -> IO LocalLocator
reflect ctx m rawTxt = do
  items <- BN.bySplit ctx m rawTxt
  case unsnoc items of
    Just (ss, base) ->
      return $
        MakeLocalLocator
          { sectionStack = map S.Section ss,
            baseName = base
          }
    Nothing ->
      -- every result of `T.split` is known to be non-empty, but unfortunately
      -- the fact is not represented in its cod type.
      Throw.raiseError ctx m $ "invalid local locator: `" <> rawTxt <> "`"

new :: [S.Section] -> BN.BaseName -> LocalLocator
new ss base =
  MakeLocalLocator
    { sectionStack = ss,
      baseName = base
    }

unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  foldr go Nothing
  where
    go x acc =
      case acc of
        Nothing ->
          Just ([], x)
        Just (ys, y) ->
          Just (x : ys, y)

extend :: LocalLocator -> LocalLocator -> LocalLocator
extend outer inner =
  MakeLocalLocator
    { sectionStack = sectionStack inner ++ [S.Section (baseName outer)] ++ sectionStack outer,
      baseName = baseName inner
    }
