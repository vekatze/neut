module Entity.Pattern
  ( Pattern (..),
    PatternRow,
    PatternMatrix,
    new,
    getHeadConstructors,
    swapColumn,
    rowVars,
    getClauseBody,
    mapMaybeRowM,
    consRow,
    unconsRow,
    findRowM,
  )
where

import Data.List
import Data.Maybe
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.Arity qualified as A
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint hiding (new)
import Entity.Ident
import Entity.Log

data Pattern
  = Var Ident
  | WildcardVar
  | Cons DD.DefiniteDescription D.Discriminant A.Arity A.Arity [(Hint, Pattern)]
  deriving (Show)

type PatternRow a =
  (V.Vector (Hint, Pattern), a)

type PatternColumn =
  [(Hint, Pattern)]

newtype PatternMatrix a
  = MakePatternMatrix (V.Vector (PatternRow a))
  deriving (Show)

mapMaybeRowM :: Monad m => (PatternRow a -> m (Maybe (PatternRow b))) -> PatternMatrix a -> m (PatternMatrix b)
mapMaybeRowM f (MakePatternMatrix mat) = do
  mat' <- mapM f mat
  return $ MakePatternMatrix $ V.catMaybes mat'

rowVars :: V.Vector (Hint, Pattern) -> [(Hint, Ident)]
rowVars vec =
  concatMap patVars $ V.toList vec

patVars :: (Hint, Pattern) -> [(Hint, Ident)]
patVars (m, pat) =
  case pat of
    Var x ->
      [(m, x)]
    WildcardVar ->
      []
    Cons _ _ _ _ patList ->
      concatMap patVars patList

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

getHeadConstructors ::
  PatternMatrix a ->
  [(Hint, (DD.DefiniteDescription, D.Discriminant, A.Arity, A.Arity, [(Hint, Pattern)]))]
getHeadConstructors (MakePatternMatrix rows) = do
  getColumnConstructors $ mapMaybe getHeadConstructors' $ V.toList rows

getHeadConstructors' :: PatternRow a -> Maybe (Hint, Pattern)
getHeadConstructors' (rows, _) =
  case V.uncons rows of
    Just (r, _) ->
      return r
    Nothing ->
      Nothing

getColumnConstructors ::
  PatternColumn ->
  [(Hint, (DD.DefiniteDescription, D.Discriminant, A.Arity, A.Arity, [(Hint, Pattern)]))]
getColumnConstructors col =
  nubBy (\(_, (dd1, _, _, _, _)) (_, (dd2, _, _, _, _)) -> dd1 == dd2) $ mapMaybe getColumnConstructor col

getColumnConstructor ::
  (Hint, Pattern) ->
  Maybe (Hint, (DD.DefiniteDescription, D.Discriminant, A.Arity, A.Arity, [(Hint, Pattern)]))
getColumnConstructor (mPat, pat) =
  case pat of
    Cons dd disc dataArity consArity args ->
      return (mPat, (dd, disc, dataArity, consArity, args))
    _ ->
      Nothing

-- swap column i and 0.
swapColumn :: Hint -> Int -> PatternMatrix a -> Either Error (PatternMatrix a)
swapColumn m i (MakePatternMatrix rows) =
  MakePatternMatrix <$> mapM (swapColumn' m i) rows

swapColumn' :: Hint -> Int -> PatternRow a -> Either Error (PatternRow a)
swapColumn' m i (row, e) = do
  let len = V.length row
  if not (0 <= i && i < len)
    then Left $ newCritical m $ T.pack $ "the index " ++ show i ++ " exceeds the vector size " ++ show len ++ "."
    else return (V.update row $ V.fromList [(0, row V.! i), (i, row V.! 0)], e)

-- get leaf if the row doesn't contain any cons; otherwise return
-- the index of the column that contains a cons
getClauseBody :: PatternRow a -> Either (Hint, Int) ([Maybe (Hint, Ident)], a)
getClauseBody patternRow =
  case patternRow of
    (patList, e) ->
      getClauseBody' 0 [] patList e

getClauseBody' ::
  Int ->
  [Maybe (Hint, Ident)] ->
  V.Vector (Hint, Pattern) ->
  a ->
  Either (Hint, Int) ([Maybe (Hint, Ident)], a)
getClauseBody' index varList patList e =
  case V.uncons patList of
    Nothing ->
      Right (reverse varList, e)
    Just ((m, Var x), rest) ->
      getClauseBody' (index + 1) (Just (m, x) : varList) rest e
    Just ((_, WildcardVar), rest) ->
      getClauseBody' (index + 1) (Nothing : varList) rest e
    Just ((m, _), _) ->
      Left (m, index)

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
