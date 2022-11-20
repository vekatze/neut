module Entity.Pattern
  ( Pattern (..),
    PatternRow,
    PatternMatrix,
    new,
    getHeadConstructors,
    swapColumn,
    getClauseBody,
    mapMaybeRowM,
    consRow,
    unconsRow,
    findRowM,
  )
where

import qualified Context.Throw as Throw
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Entity.Arity as A
import qualified Entity.DefiniteDescription as DD
import Entity.Hint hiding (new)
import Entity.Ident

data Pattern
  = Var Ident
  | WildcardVar
  | Cons DD.DefiniteDescription A.Arity [(Hint, Pattern)]

type PatternRow a =
  (V.Vector (Hint, Pattern), a)

type PatternColumn =
  [(Hint, Pattern)]

newtype PatternMatrix a
  = MakePatternMatrix (V.Vector (PatternRow a))

mapMaybeRowM :: Monad m => (PatternRow a -> m (Maybe (PatternRow b))) -> PatternMatrix a -> m (PatternMatrix b)
mapMaybeRowM f (MakePatternMatrix mat) = do
  mat' <- mapM f mat
  return $ MakePatternMatrix $ V.catMaybes mat'

consRow :: PatternRow a -> PatternMatrix a -> PatternMatrix a
consRow row (MakePatternMatrix mat) =
  MakePatternMatrix $ V.cons row mat

unconsRow :: PatternMatrix a -> Maybe (PatternRow a, PatternMatrix a)
unconsRow (MakePatternMatrix mat) = do
  (headRow, rest) <- V.uncons mat
  return (headRow, MakePatternMatrix rest)

new :: [PatternRow a] -> PatternMatrix a
new rows =
  MakePatternMatrix $ V.fromList rows

getHeadConstructors :: PatternMatrix a -> S.Set (DD.DefiniteDescription, A.Arity)
getHeadConstructors (MakePatternMatrix rows) = do
  getColumnConstructors $ mapMaybe getHeadConstructors' $ V.toList rows

getHeadConstructors' :: PatternRow a -> Maybe (Hint, Pattern)
getHeadConstructors' (rows, _) =
  case V.uncons rows of
    Just (r, _) ->
      return r
    Nothing ->
      Nothing

getColumnConstructors :: PatternColumn -> S.Set (DD.DefiniteDescription, A.Arity)
getColumnConstructors col =
  S.fromList $ mapMaybe (getColumnConstructor . snd) col

getColumnConstructor :: Pattern -> Maybe (DD.DefiniteDescription, A.Arity)
getColumnConstructor pat =
  case pat of
    Cons dd arity _ ->
      return (dd, arity)
    _ ->
      Nothing

-- swap column i and 0.
swapColumn :: Throw.Context m => Hint -> Int -> PatternMatrix a -> m (PatternMatrix a)
swapColumn m i (MakePatternMatrix rows) =
  MakePatternMatrix <$> mapM (swapColumn' m i) rows

swapColumn' :: Throw.Context m => Hint -> Int -> PatternRow a -> m (PatternRow a)
swapColumn' m i (row, e) = do
  let len = V.length row
  if not (0 <= i && i < len)
    then Throw.raiseCritical m $ T.pack $ "the index " ++ show i ++ " exceeds the vector size " ++ show len ++ "."
    else return (V.update row $ V.fromList [(0, row V.! i), (i, row V.! 0)], e)

-- get leaf if the first row doesn't contain any cons; otherwise return
-- the index of the column that contains a cons
getClauseBody :: PatternRow a -> Either Int ([Maybe Ident], a)
getClauseBody patternRow =
  case patternRow of
    (patList, e) ->
      getClauseBody' 0 [] patList e

getClauseBody' :: Int -> [Maybe Ident] -> V.Vector (Hint, Pattern) -> a -> Either Int ([Maybe Ident], a)
getClauseBody' index varList patList e =
  case V.uncons patList of
    Nothing ->
      Right (reverse varList, e)
    Just ((_, Var x), rest) ->
      getClauseBody' (index + 1) (Just x : varList) rest e
    Just ((_, WildcardVar), rest) ->
      getClauseBody' (index + 1) (Nothing : varList) rest e
    _ ->
      Left index

findRowM :: Monad m => (PatternRow a -> m (Maybe b)) -> PatternMatrix a -> m (Maybe b)
findRowM f (MakePatternMatrix rows) =
  case V.uncons rows of
    Just (row, rest) -> do
      mv <- f row
      case mv of
        Just v ->
          return $ Just v
        Nothing ->
          findRowM f (MakePatternMatrix rest)
    Nothing ->
      return Nothing
