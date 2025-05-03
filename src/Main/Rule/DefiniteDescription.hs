module Main.Rule.DefiniteDescription
  ( DefiniteDescription (..),
    new,
    moduleID,
    localLocator,
    globalLocator,
    getReadableDD,
    getLocatorPair,
    newByGlobalLocator,
    getFormDD,
    imm,
    cls,
    toBuilder,
    llvmGlobalLocator,
    isEntryPoint,
  )
where

import Data.Binary
import Data.ByteString.Builder
import Data.HashMap.Strict qualified as Map
import Data.Hashable
import Data.List (find)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics
import Main.Rule.BaseName qualified as BN
import Main.Rule.Const
import Main.Rule.Error
import Main.Rule.GlobalLocator qualified as GL
import Main.Rule.Hint qualified as H
import Main.Rule.List (initLast)
import Main.Rule.LocalLocator qualified as LL
import Main.Rule.Module qualified as M
import Main.Rule.ModuleAlias qualified as MA
import Main.Rule.ModuleDigest qualified as MD
import Main.Rule.ModuleID qualified as MID
import Main.Rule.SourceLocator qualified as SL
import Main.Rule.StrictGlobalLocator qualified as SGL

newtype DefiniteDescription = MakeDefiniteDescription {reify :: T.Text}
  deriving (Generic, Show)

instance Eq DefiniteDescription where
  dd1 == dd2 = do
    reify dd1 == reify dd2

instance Ord DefiniteDescription where
  compare dd1 dd2 = compare (reify dd1) (reify dd2)

instance Binary DefiniteDescription

instance Hashable DefiniteDescription

new :: SGL.StrictGlobalLocator -> LL.LocalLocator -> DefiniteDescription
new gl ll =
  MakeDefiniteDescription
    { reify = SGL.reify gl <> nsSep <> LL.reify ll
    }

newByGlobalLocator :: SGL.StrictGlobalLocator -> BN.BaseName -> DefiniteDescription
newByGlobalLocator gl name = do
  new gl $ LL.new name

{-# INLINE toLowName #-}
toLowName :: DefiniteDescription -> T.Text
toLowName dd =
  wrapWithQuote $ reify dd

{-# INLINE wrapWithQuote #-}
wrapWithQuote :: T.Text -> T.Text
wrapWithQuote x =
  "\"" <> x <> "\""

-- this.foo.bar
-- ~> this.foo.bar#form
getFormDD :: DefiniteDescription -> DefiniteDescription
getFormDD dd = do
  MakeDefiniteDescription
    { reify = reify dd <> "#" <> BN.reify BN.form
    }

moduleID :: DefiniteDescription -> T.Text
moduleID dd = do
  let nameList = T.splitOn nsSep (reify dd)
  case nameList of
    headElem : _ ->
      headElem
    _ ->
      error "Rule.DefiniteDescription.moduleID"

unconsDD :: DefiniteDescription -> (MID.ModuleID, T.Text)
unconsDD dd = do
  let nameList = T.splitOn nsSep (reify dd)
  case nameList of
    headElem : rest ->
      case headElem of
        "this" ->
          (MID.Main, T.intercalate nsSep rest)
        "base" ->
          (MID.Base, T.intercalate nsSep rest)
        _ ->
          (MID.Library (MD.ModuleDigest headElem), T.intercalate nsSep rest)
    _ ->
      error "Rule.DefiniteDescription.moduleID"

getReadableDD :: M.Module -> DefiniteDescription -> T.Text
getReadableDD baseModule dd =
  case unconsDD dd of
    (MID.Main, rest) ->
      "this" <> nsSep <> rest
    (MID.Base, rest) ->
      "base" <> nsSep <> rest
    (MID.Library digest, rest) -> do
      let depMap = Map.toList $ M.moduleDependency baseModule
      let aliasOrNone = fmap (MA.reify . fst) $ flip find depMap $ \(_, dependency) -> do
            digest == M.dependencyDigest dependency
      case aliasOrNone of
        Nothing ->
          reify dd
        Just alias ->
          alias <> nsSep <> rest

globalLocator :: DefiniteDescription -> T.Text
globalLocator dd = do
  let nameList = T.splitOn nsSep (reify dd)
  case initLast nameList of
    Just (xs, _) ->
      T.intercalate nsSep xs
    _ ->
      error "Rule.DefiniteDescription.globalLocator"

localLocator :: DefiniteDescription -> T.Text
localLocator dd = do
  let nameList = T.splitOn "." (reify dd)
  case initLast nameList of
    Just (_, result) ->
      result
    _ ->
      error "Rule.DefiniteDescription.localLocator"

imm :: DefiniteDescription
imm =
  newByGlobalLocator (SGL.baseGlobalLocatorOf SL.internalLocator) BN.imm

cls :: DefiniteDescription
cls =
  newByGlobalLocator (SGL.baseGlobalLocatorOf SL.internalLocator) BN.cls

toBuilder :: DefiniteDescription -> Builder
toBuilder dd =
  TE.encodeUtf8Builder $ toLowName dd

getLocatorPair :: H.Hint -> T.Text -> Either Error (GL.GlobalLocator, LL.LocalLocator)
getLocatorPair m varText = do
  let nameList = T.splitOn "." varText
  case initLast nameList of
    Nothing ->
      Left $ newError m "Rule.DefiniteDescription.getLocatorPair: empty variable name"
    Just ([], _) ->
      Left $ newError m $ "The symbol `" <> varText <> "` does not contain a global locator"
    Just (initElems, lastElem) -> do
      gl <- GL.reflect m $ T.intercalate "." initElems
      ll <- LL.reflect m lastElem
      return (gl, ll)

llvmGlobalLocator :: T.Text
llvmGlobalLocator =
  "base.llvm"

isEntryPoint :: DefiniteDescription -> Bool
isEntryPoint dd =
  localLocator dd `elem` ["main", "zen"]
