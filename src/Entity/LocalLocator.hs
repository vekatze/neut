module Entity.LocalLocator
  ( LocalLocator (..),
    new,
    reify,
    reflect,
    extend,
  )
where

import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.Hint qualified as H
import Entity.Log
import Entity.Section qualified as S
import GHC.Generics

data LocalLocator = MakeLocalLocator
  { sectionStack :: [S.Section],
    baseName :: BN.BaseName
  }
  deriving (Generic, Show, Eq)

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

reflect :: H.Hint -> T.Text -> Either Error LocalLocator
reflect m rawTxt = do
  items <- BN.bySplit m rawTxt
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
      Left $ newCritical m $ "invalid local locator: `" <> rawTxt <> "`"

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
