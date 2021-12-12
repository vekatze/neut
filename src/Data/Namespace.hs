{-# LANGUAGE TupleSections #-}

module Data.Namespace where

import Data.Basic (EnumCase (EnumCaseLabel), Hint, Ident, TopName)
import Data.Global (aliasEnv, prefixEnv, sectionEnv)
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef)
import Data.Log (raiseError)
import Data.LowType
  ( LowType (LowTypeFloat, LowTypeInt),
    asLowTypeMaybe,
    asPrimOp,
  )
import qualified Data.Text as T
import Data.WeakTerm
  ( WeakTerm
      ( WeakTermConst,
        WeakTermEnum,
        WeakTermEnumIntro,
        WeakTermVar,
        WeakTermVarGlobal
      ),
    WeakTermPlus,
  )

nsSepChar :: Char
nsSepChar =
  '.'

{-# INLINE nsSep #-}
nsSep :: T.Text
nsSep =
  T.singleton nsSepChar

{-# INLINE nsUnsafe #-}
nsUnsafe :: T.Text
nsUnsafe =
  "unsafe" <> nsSep

{-# INLINE nsOS #-}
nsOS :: T.Text
nsOS =
  "os" <> nsSep

withSectionPrefix :: T.Text -> IO T.Text
withSectionPrefix x = do
  ns <- readIORef sectionEnv
  return $ foldl (\acc n -> n <> nsSep <> acc) x ns

getCurrentSection :: IO T.Text
getCurrentSection = do
  ns <- readIORef sectionEnv
  return $ getCurrentSection' ns

getCurrentSection' :: [T.Text] -> T.Text
getCurrentSection' nameStack =
  case nameStack of
    [] ->
      ""
    [n] ->
      n
    (n : ns) ->
      getCurrentSection' ns <> nsSep <> n

handleSection :: T.Text -> IO ()
handleSection s = do
  modifyIORef' sectionEnv $ \env -> s : env
  handleUse s

handleUse :: T.Text -> IO ()
handleUse s =
  modifyIORef' prefixEnv $ \env -> s : env

handleDefinePrefix :: T.Text -> T.Text -> IO ()
handleDefinePrefix from to = do
  modifyIORef' aliasEnv $ \env -> (from, to) : env

{-# INLINE resolveSymbol #-}
resolveSymbol :: Hint -> (T.Text -> Maybe b) -> T.Text -> IO (Maybe b)
resolveSymbol m predicate name = do
  candList <- constructCandList name
  case takeAll predicate candList [] of
    [] ->
      return Nothing
    [prefixedName] ->
      return $ predicate prefixedName
    candList' -> do
      let candInfo = T.concat $ map ("\n- " <>) candList'
      raiseError m $ "this `" <> name <> "` is ambiguous since it could refer to:" <> candInfo

constructCandList :: T.Text -> IO [T.Text]
constructCandList name = do
  penv <- readIORef prefixEnv
  nenv <- readIORef aliasEnv
  return $ constructCandList' nenv $ name : map (<> nsSep <> name) penv

constructCandList' :: [(T.Text, T.Text)] -> [T.Text] -> [T.Text]
constructCandList' nenv =
  concatMap (constructCandList'' nenv)

constructCandList'' :: [(T.Text, T.Text)] -> T.Text -> [T.Text]
constructCandList'' nenv name = do
  let nameList = findNext nenv name
  if null nameList
    then [name]
    else constructCandList' nenv nameList

findNext :: [(T.Text, T.Text)] -> T.Text -> [T.Text]
findNext nenv name = do
  concat $
    concat $
      flip map nenv $ \(from, to) -> do
        case T.stripPrefix (from <> nsSep) name of
          Just suffix -> do
            return [to <> nsSep <> suffix]
          Nothing ->
            return []

takeAll :: (T.Text -> Maybe b) -> [T.Text] -> [T.Text] -> [T.Text]
takeAll predicate candidateList acc =
  case candidateList of
    [] ->
      acc
    x : xs -> do
      case predicate x of
        Just _ ->
          takeAll predicate xs (x : acc)
        Nothing ->
          takeAll predicate xs acc

{-# INLINE asVar #-}
asVar :: Hint -> Map.HashMap T.Text b -> T.Text -> (b -> a) -> Maybe (Hint, a)
asVar m nenv name f =
  Map.lookup name nenv >>= \x -> return (m, f x)

{-# INLINE asWeakVar #-}
asWeakVar :: Hint -> Map.HashMap T.Text Ident -> T.Text -> Maybe WeakTermPlus
asWeakVar m nenv var =
  asVar m nenv var WeakTermVar

{-# INLINE asGlobalVar #-}
asGlobalVar :: Hint -> Map.HashMap T.Text FilePath -> T.Text -> Maybe WeakTermPlus
asGlobalVar m nenv name =
  asVar m nenv name (\fp -> WeakTermVarGlobal (fp, name))

{-# INLINE asConstructor #-}
asConstructor :: Hint -> Map.HashMap T.Text FilePath -> T.Text -> Maybe (Hint, TopName)
asConstructor m nenv name =
  asVar m nenv name (,name)

{-# INLINE findThenModify #-}
findThenModify :: Map.HashMap T.Text t -> (T.Text -> a) -> T.Text -> Maybe a
findThenModify env f name = do
  if name `Map.member` env
    then Just $ f name
    else Nothing

{-# INLINE asEnumLabel #-}
asEnumLabel :: Map.HashMap T.Text (FilePath, T.Text, Int) -> T.Text -> Maybe EnumCase
asEnumLabel env name = do
  case Map.lookup name env of
    Just (fp, _, _) ->
      Just $ EnumCaseLabel fp name
    _ ->
      Nothing

{-# INLINE asEnumIntro #-}
asEnumIntro :: Hint -> Map.HashMap T.Text (FilePath, T.Text, Int) -> T.Text -> Maybe WeakTermPlus
asEnumIntro m env name = do
  case Map.lookup name env of
    Just (fp, _, _) ->
      Just (m, WeakTermEnumIntro fp name)
    _ ->
      Nothing

{-# INLINE asEnum #-}
asEnum :: Hint -> Map.HashMap T.Text (FilePath, a) -> T.Text -> Maybe WeakTermPlus
asEnum m env name = do
  case Map.lookup name env of
    Just (fp, _) ->
      Just (m, WeakTermEnum fp name)
    _ ->
      Nothing

{-# INLINE asWeakConstant #-}
asWeakConstant :: Hint -> T.Text -> Maybe WeakTermPlus
asWeakConstant m name
  | Just (LowTypeInt _) <- asLowTypeMaybe name =
    Just (m, WeakTermConst name)
  | Just (LowTypeFloat _) <- asLowTypeMaybe name =
    Just (m, WeakTermConst name)
  | Just _ <- asPrimOp name =
    Just (m, WeakTermConst name)
  | otherwise = do
    Nothing

tryCand :: (Monad m) => m (Maybe a) -> m a -> m a
tryCand comp cont = do
  mx <- comp
  maybe cont return mx
