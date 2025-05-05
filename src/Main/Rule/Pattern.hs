module Main.Rule.Pattern
  ( Pattern (..),
    PatternRow,
    PatternMatrix,
    ConsInfo (..),
    Specializer (..),
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
import Error.Rule.Error
import Language.Common.Rule.ArgNum qualified as AN
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Discriminant qualified as D
import Language.Common.Rule.Error
import Language.Common.Rule.Hint
import Language.Common.Rule.Ident
import Language.Common.Rule.IsConstLike
import Language.Common.Rule.Literal qualified as L

data Pattern
  = Var Ident
  | WildcardVar
  | Cons ConsInfo
  | Literal L.Literal
  deriving (Show)

data ConsInfo = ConsInfo
  { consDD :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    disc :: D.Discriminant,
    dataArgNum :: AN.ArgNum,
    consArgNum :: AN.ArgNum,
    args :: [(Hint, Pattern)]
  }
  deriving (Show)

data Specializer
  = ConsSpecializer ConsInfo
  | LiteralSpecializer L.Literal

type PatternRow a =
  (V.Vector (Hint, Pattern), a)

type PatternColumn =
  [(Hint, Pattern)]

newtype PatternMatrix a
  = MakePatternMatrix (V.Vector (PatternRow a))
  deriving (Show)

mapMaybeRowM :: (Monad m) => (PatternRow a -> m (Maybe (PatternRow b))) -> PatternMatrix a -> m (PatternMatrix b)
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
    Literal _ ->
      []
    WildcardVar ->
      []
    Cons consInfo ->
      concatMap patVars (args consInfo)

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
  [(Hint, Specializer)]
getHeadConstructors (MakePatternMatrix rows) = do
  getColumnConstructors $ mapMaybe getHeadConstructors' $ V.toList rows

getHeadConstructors' :: PatternRow a -> Maybe (Hint, Pattern)
getHeadConstructors' (rows, _) =
  case V.uncons rows of
    Just (r, _) ->
      return r
    Nothing ->
      Nothing

data PseudoDD
  = LiteralDD L.Literal
  | ConsDD DD.DefiniteDescription
  deriving (Eq, Ord)

consInfoToDD :: (Hint, Specializer) -> PseudoDD
consInfoToDD (_, specializer) =
  case specializer of
    LiteralSpecializer i ->
      LiteralDD i
    ConsSpecializer consInfo ->
      ConsDD $ consDD consInfo

getColumnConstructors :: PatternColumn -> [(Hint, Specializer)]
getColumnConstructors col = do
  ListUtils.nubOrdOn consInfoToDD $ mapMaybe getColumnConstructor col

getColumnConstructor ::
  (Hint, Pattern) ->
  Maybe (Hint, Specializer)
getColumnConstructor (mPat, pat) =
  case pat of
    Literal l ->
      return (mPat, LiteralSpecializer l)
    Cons consInfo ->
      return (mPat, ConsSpecializer consInfo)
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

findRowM :: (Monad m) => (PatternRow a -> m (Maybe b)) -> PatternMatrix a -> m (Maybe b)
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
