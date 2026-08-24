module Language.Common.CallConv
  ( CallConv,
    Argument (..),
    normal,
    destination,
    fromPiKind,
    fromNoeticPiKind,
    withArguments,
    argumentsFor,
    sourceFlags,
    types,
    mapTypes,
    traverseTypes,
    isNormal,
    isDestPassing,
    isNoetic,
    destinationType,
  )
where

import Data.Binary (Binary)
import Data.Foldable (toList)
import GHC.Generics (Generic)
import Language.Common.PiKind qualified as PK

data Evaluation
  = Runtime
  | Noetic
  deriving (Eq, Ord, Show, Generic)

instance Binary Evaluation

data Result t
  = Return
  | Destination t
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (Binary t) => Binary (Result t)

data Argument t
  = Plain
  | Source t
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (Binary t) => Binary (Argument t)

data CallConv t = CallConv Evaluation (Result t) [Argument t]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (Binary t) => Binary (CallConv t)

normal :: CallConv t
normal =
  CallConv Runtime Return []

destination :: t -> CallConv t
destination t =
  CallConv Runtime (Destination t) []

fromPiKind :: PK.PiKind -> t -> CallConv t
fromPiKind piKind t =
  CallConv Runtime (resultFromPiKind piKind t) []

fromNoeticPiKind :: PK.PiKind -> t -> CallConv t
fromNoeticPiKind piKind t =
  CallConv Noetic (resultFromPiKind piKind t) []

resultFromPiKind :: PK.PiKind -> t -> Result t
resultFromPiKind piKind t =
  case piKind of
    PK.DestPass _ ->
      Destination t
    _ ->
      Return

withArguments :: [Argument t] -> CallConv t -> CallConv t
withArguments args (CallConv evaluation result _) = do
  let args' = if any isSource args then args else [] -- optimization; not necessary for correctness
  CallConv evaluation result args'

isSource :: Argument t -> Bool
isSource argument =
  case argument of
    Source _ ->
      True
    Plain ->
      False

argumentsFor :: Int -> CallConv t -> [Argument t]
argumentsFor argCount (CallConv _ _ args) =
  take argCount $ args ++ repeat Plain

sourceFlags :: Int -> CallConv t -> [Bool]
sourceFlags argCount conv =
  map isSource $ argumentsFor argCount conv

types :: CallConv t -> [t]
types =
  toList

mapTypes :: (a -> b) -> CallConv a -> CallConv b
mapTypes =
  fmap

traverseTypes :: (Applicative f) => (a -> f b) -> CallConv a -> f (CallConv b)
traverseTypes f conv =
  case conv of
    CallConv evaluation Return [] ->
      pure $ CallConv evaluation Return []
    _ ->
      traverse f conv

isNormal :: CallConv t -> Bool
isNormal conv =
  case conv of
    CallConv Runtime Return [] ->
      True
    _ ->
      False

isNoetic :: CallConv t -> Bool
isNoetic (CallConv evaluation _ _) =
  case evaluation of
    Runtime ->
      False
    Noetic ->
      True

isDestPassing :: CallConv t -> Bool
isDestPassing (CallConv _ result _) =
  case result of
    Return ->
      False
    Destination _ ->
      True

destinationType :: CallConv t -> Maybe t
destinationType (CallConv _ result _) =
  case result of
    Return ->
      Nothing
    Destination t ->
      Just t
