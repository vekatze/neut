module Entity.Pattern
  ( Pattern (..),
    PatternRow,
    PatternMatrix,
    ConsInfo,
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

import Data.Containers.ListUtils qualified as ListUtils
import Data.Maybe
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.ArgNum qualified as AN
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Error
import Entity.Hint hiding (new)
import Entity.Ident

data Pattern
  = Var Ident
  | WildcardVar
  | Cons DD.DefiniteDescription D.Discriminant AN.ArgNum AN.ArgNum [(Hint, Pattern)]
  | NatZero
  | NatSucc (Hint, Pattern)
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
    NatZero ->
      []
    NatSucc pat' ->
      patVars pat'

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
  [ConsInfo]
getHeadConstructors (MakePatternMatrix rows) = do
  getColumnConstructors $ mapMaybe getHeadConstructors' $ V.toList rows

getHeadConstructors' :: PatternRow a -> Maybe (Hint, Pattern)
getHeadConstructors' (rows, _) =
  case V.uncons rows of
    Just (r, _) ->
      return r
    Nothing ->
      Nothing

type ConsInfo =
  (Hint, (DD.DefiniteDescription, D.Discriminant, AN.ArgNum, AN.ArgNum, [(Hint, Pattern)]))

consInfoToDD :: ConsInfo -> DD.DefiniteDescription
consInfoToDD consInfo =
  case consInfo of
    (_, (dd, _, _, _, _)) ->
      dd

getColumnConstructors :: PatternColumn -> [ConsInfo]
getColumnConstructors col =
  ListUtils.nubOrdOn consInfoToDD $ mapMaybe getColumnConstructor col

getColumnConstructor ::
  (Hint, Pattern) ->
  Maybe ConsInfo
getColumnConstructor (mPat, pat) =
  case pat of
    Cons dd disc dataArgNum consArgNum args ->
      return (mPat, (dd, disc, dataArgNum, consArgNum, args))
    NatZero ->
      return (mPat, (DD.natZero, D.MakeDiscriminant 0, AN.fromInt 0, AN.fromInt 0, []))
    NatSucc arg ->
      return (mPat, (DD.natSucc, D.MakeDiscriminant 1, AN.fromInt 0, AN.fromInt 1, [arg]))
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
