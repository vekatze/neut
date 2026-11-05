module Binary.Derive (deriveBinary) where

import Control.Monad
import Data.Binary (Binary (..), getWord8, putWord8)
import Language.Haskell.TH

deriveBinary :: [Name] -> Q [Dec]
deriveBinary =
  fmap concat . mapM deriveBinaryInstance

deriveBinaryInstance :: Name -> Q [Dec]
deriveBinaryInstance typeName = do
  (binders, constructors) <- reifyDataType typeName
  shapes <- mapM (toShape typeName) constructors
  when (length shapes > 256) $
    fail $
      "deriveBinary: too many constructors in " <> show typeName
  let typeVars = map VarT binders
  let context = map (AppT (ConT ''Binary)) typeVars
  let instanceType = AppT (ConT ''Binary) (foldl AppT (ConT typeName) typeVars)
  putClauses <- zipWithM (makePutClause (length shapes > 1)) [0 ..] shapes
  getBody <- makeGetBody typeName shapes
  return
    [ InstanceD
        Nothing
        context
        instanceType
        [ FunD 'put putClauses,
          ValD (VarP 'get) (NormalB getBody) []
        ]
    ]

data Shape = Shape
  { shapeName :: Name,
    shapeArity :: Int
  }

reifyDataType :: Name -> Q ([Name], [Con])
reifyDataType typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ binders _ constructors _) ->
      return (map binderName binders, constructors)
    TyConI (NewtypeD _ _ binders _ constructor _) ->
      return (map binderName binders, [constructor])
    _ ->
      fail $ "deriveBinary: not a data type: " <> show typeName

binderName :: TyVarBndr flag -> Name
binderName binder =
  case binder of
    PlainTV name _ ->
      name
    KindedTV name _ _ ->
      name

toShape :: Name -> Con -> Q Shape
toShape typeName constructor =
  case constructor of
    NormalC name fields ->
      return $ Shape name (length fields)
    RecC name fields ->
      return $ Shape name (length fields)
    InfixC _ name _ ->
      return $ Shape name 2
    _ ->
      fail $ "deriveBinary: unsupported constructor in " <> show typeName

makePutClause :: Bool -> Integer -> Shape -> Q Clause
makePutClause hasTag tag shape = do
  fieldNames <- replicateM (shapeArity shape) (newName "field")
  let tagPut = [AppE (VarE 'putWord8) (LitE (IntegerL tag)) | hasTag]
  let fieldPuts = map (AppE (VarE 'put) . VarE) fieldNames
  let body = sequenceActions (tagPut ++ fieldPuts)
  return $ Clause [ConP (shapeName shape) [] (map VarP fieldNames)] (NormalB body) []

sequenceActions :: [Exp] -> Exp
sequenceActions actions =
  case actions of
    [] ->
      AppE (VarE 'return) (TupE [])
    _ ->
      foldr1 (\action rest -> InfixE (Just action) (VarE '(>>)) (Just rest)) actions

makeGetBody :: Name -> [Shape] -> Q Exp
makeGetBody typeName shapes =
  case shapes of
    [shape] ->
      return $ getFields shape
    _ -> do
      tagName <- newName "tag"
      let matches = zipWith makeGetMatch [0 ..] shapes
      let failure = AppE (VarE 'fail) (LitE (StringL ("invalid tag for " <> show typeName)))
      let dispatch = CaseE (VarE tagName) (matches ++ [Match WildP (NormalB failure) []])
      return $ InfixE (Just (VarE 'getWord8)) (VarE '(>>=)) (Just (LamE [VarP tagName] dispatch))

makeGetMatch :: Integer -> Shape -> Match
makeGetMatch tag shape =
  Match (LitP (IntegerL tag)) (NormalB (getFields shape)) []

getFields :: Shape -> Exp
getFields shape =
  applyGets (ConE (shapeName shape)) (shapeArity shape)

applyGets :: Exp -> Int -> Exp
applyGets constructor count =
  case count of
    0 ->
      AppE (VarE 'pure) constructor
    _ ->
      InfixE (Just (applyGets constructor (count - 1))) (VarE '(<*>)) (Just (VarE 'get))
