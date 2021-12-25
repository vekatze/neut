{-# LANGUAGE TupleSections #-}

module Data.Namespace where

import Control.Comonad.Cofree (Cofree (..))
import Data.Basic (EnumCase, EnumCaseF (EnumCaseLabel), Hint, Ident)
import Data.Global (aliasEnv, nsSep, prefixEnv, sectionEnv)
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef)
import Data.Log (raiseError)
import Data.LowType
  ( LowType (LowTypeFloat, LowTypeInt),
    asLowTypeMaybe,
    asPrimOp,
  )
import qualified Data.Set as S
import qualified Data.Text as T
import Data.WeakTerm
  ( WeakTerm,
    WeakTermF
      ( WeakTermConst,
        WeakTermEnum,
        WeakTermEnumIntro,
        WeakTermVar,
        WeakTermVarGlobal
      ),
  )

data Section
  = Section T.Text [T.Text]

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
  return $ T.intercalate "." (ns ++ [x])

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
-- asVar :: Hint -> Map.HashMap T.Text b -> T.Text -> (b -> a) -> Maybe (Cofree a Hint)
asVar :: Hint -> Map.HashMap T.Text t -> T.Text -> (t -> f (Cofree f Hint)) -> Maybe (Cofree f Hint)
asVar m nenv name f =
  Map.lookup name nenv >>= \x -> return (m :< f x)

{-# INLINE asWeakVar #-}
asWeakVar :: Hint -> Map.HashMap T.Text Ident -> T.Text -> Maybe WeakTerm
asWeakVar m nenv var =
  asVar m nenv var WeakTermVar

{-# INLINE asGlobalVar #-}
asGlobalVar :: Hint -> S.Set T.Text -> T.Text -> Maybe WeakTerm
asGlobalVar m nenv name =
  if S.member name nenv
    then Just (m :< WeakTermVarGlobal name)
    else Nothing

{-# INLINE asConstructor #-}
asConstructor :: Hint -> S.Set T.Text -> T.Text -> Maybe (Hint, T.Text)
asConstructor m nenv name =
  if S.member name nenv
    then Just (m, name)
    else Nothing

{-# INLINE findThenModify #-}
findThenModify :: Map.HashMap T.Text t -> (T.Text -> a) -> T.Text -> Maybe a
findThenModify env f name = do
  if name `Map.member` env
    then Just $ f name
    else Nothing

{-# INLINE asEnumLabel #-}
asEnumLabel :: Hint -> Map.HashMap T.Text (T.Text, Int) -> T.Text -> Maybe EnumCase
asEnumLabel m env name = do
  case Map.lookup name env of
    Just _ ->
      Just $ m :< EnumCaseLabel name
    _ ->
      Nothing

{-# INLINE asEnumIntro #-}
asEnumIntro :: Hint -> Map.HashMap T.Text (T.Text, Int) -> T.Text -> Maybe WeakTerm
asEnumIntro m env name = do
  case Map.lookup name env of
    Just (_, _) ->
      Just (m :< WeakTermEnumIntro name)
    _ ->
      Nothing

{-# INLINE asEnum #-}
asEnum :: Hint -> Map.HashMap T.Text a -> T.Text -> Maybe WeakTerm
asEnum m env name = do
  case Map.lookup name env of
    Just _ ->
      Just (m :< WeakTermEnum name)
    _ ->
      Nothing

{-# INLINE asWeakConstant #-}
asWeakConstant :: Hint -> T.Text -> Maybe WeakTerm
asWeakConstant m name
  | Just (LowTypeInt _) <- asLowTypeMaybe name =
    Just (m :< WeakTermConst name)
  | Just (LowTypeFloat _) <- asLowTypeMaybe name =
    Just (m :< WeakTermConst name)
  | Just _ <- asPrimOp name =
    Just (m :< WeakTermConst name)
  | otherwise = do
    Nothing

tryCand :: (Monad m) => m (Maybe a) -> m a -> m a
tryCand comp cont = do
  mx <- comp
  maybe cont return mx
