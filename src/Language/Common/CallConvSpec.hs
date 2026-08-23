module Language.Common.CallConvSpec
  ( CallConvSpec (..),
    marks,
    types,
    traverseTypes,
  )
where

import Data.Foldable (toList)
import Language.Common.CallConv qualified as CC
import Language.Common.CallSite (IsDestCall, IsSourceArg)

data CallConvSpec t
  = AsMarked IsDestCall [IsSourceArg]
  | Inferred (CC.CallConv t)
  deriving (Eq, Show, Functor, Foldable, Traversable)

marks :: Int -> CallConvSpec t -> (IsDestCall, [IsSourceArg])
marks argCount spec =
  case spec of
    AsMarked isDestCall sourceArgs ->
      (isDestCall, take argCount $ sourceArgs ++ repeat False)
    Inferred conv ->
      (CC.isDestPassing conv, CC.sourceFlags argCount conv)

types :: CallConvSpec t -> [t]
types =
  toList

traverseTypes :: (Applicative f) => (a -> f b) -> CallConvSpec a -> f (CallConvSpec b)
traverseTypes f spec =
  case spec of
    AsMarked isDestCall sourceArgs ->
      pure $ AsMarked isDestCall sourceArgs
    Inferred conv ->
      Inferred <$> CC.traverseTypes f conv
