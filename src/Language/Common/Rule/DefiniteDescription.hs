module Language.Common.Rule.DefiniteDescription
  ( DefiniteDescription (..),
    new,
    moduleID,
    localLocator,
    globalLocator,
    getLocatorPair,
    newByGlobalLocator,
    getFormDD,
    imm,
    immType,
    immEnum,
    cls,
    toBuilder,
    llvmGlobalLocator,
    unconsDD,
  )
where

import Data.Binary
import Data.ByteString.Builder
import Data.Hashable
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Error.Rule.Error
import GHC.Generics
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.Const
import Language.Common.Rule.GlobalLocator qualified as GL
import Language.Common.Rule.List (initLast)
import Language.Common.Rule.LocalLocator qualified as LL
import Language.Common.Rule.ModuleDigest qualified as MD
import Language.Common.Rule.ModuleID qualified as MID
import Language.Common.Rule.SourceLocator qualified as SL
import Language.Common.Rule.StrictGlobalLocator qualified as SGL
import Logger.Rule.Hint qualified as H

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

immType :: DefiniteDescription
immType =
  newByGlobalLocator (SGL.baseGlobalLocatorOf SL.internalLocator) BN.immType

immEnum :: DefiniteDescription
immEnum =
  newByGlobalLocator (SGL.baseGlobalLocatorOf SL.internalLocator) BN.immEnum

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
