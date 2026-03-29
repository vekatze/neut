module Language.Common.DefiniteDescription
  ( DefiniteDescription (..),
    new,
    localLocator,
    globalLocator,
    getLocatorPair,
    newByGlobalLocator,
    getFormDD,
    getNodeDD,
    getLeafDD,
    getRootDD,
    getClosureEnvDD,
    getLambdaDD,
    getMuDD,
    imm,
    baseTypes,
    cls,
    toBuilder,
    llvmGlobalLocator,
    unconsDD,
    makeResourceName,
  )
where

import App.Error
import Data.Binary
import Data.ByteString.Builder
import Data.Hashable
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics
import Language.Common.BaseName qualified as BN
import Language.Common.Const
import Language.Common.GlobalLocator qualified as GL
import Language.Common.Ident
import Language.Common.List (initLast)
import Language.Common.LocalLocator qualified as LL
import Language.Common.ModuleDigest qualified as MD
import Language.Common.ModuleID qualified as MID
import Language.Common.SourceLocator qualified as SL
import Language.Common.StrictGlobalLocator qualified as SGL
import Logger.Hint qualified as H

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
appendLocalName :: DefiniteDescription -> BN.BaseName -> DefiniteDescription
appendLocalName dd name = do
  MakeDefiniteDescription
    { reify = reify dd <> "#" <> BN.reify name
    }

getFormDD :: DefiniteDescription -> DefiniteDescription
getFormDD dd =
  appendLocalName dd BN.form

getNodeDD :: DefiniteDescription -> DefiniteDescription
getNodeDD dd =
  appendLocalName dd BN.node

getLeafDD :: DefiniteDescription -> DefiniteDescription
getLeafDD dd =
  appendLocalName dd BN.leaf

getRootDD :: DefiniteDescription -> DefiniteDescription
getRootDD dd =
  appendLocalName dd BN.root

getClosureEnvDD :: DefiniteDescription -> Int -> DefiniteDescription
getClosureEnvDD closureName i =
  MakeDefiniteDescription
    { reify = reify closureName <> ";env;" <> T.pack (show i)
    }

getLambdaDD :: DefiniteDescription -> Maybe T.Text -> Int -> DefiniteDescription
getLambdaDD dd mName i =
  appendLocalName dd (BN.lambdaName mName i)

getMuDD :: DefiniteDescription -> Ident -> Int -> DefiniteDescription
getMuDD dd x i =
  appendLocalName dd (BN.muName x i)

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

makeResourceName :: DefiniteDescription -> Int -> DefiniteDescription
makeResourceName baseDD resourceID = do
  let gl = globalLocator baseDD
  MakeDefiniteDescription $ gl <> nsSep <> BN.reify (BN.resourceName resourceID)

imm :: DefiniteDescription
imm =
  newByGlobalLocator (SGL.baseGlobalLocatorOf SL.internalLocator) BN.imm

baseTypes :: [DefiniteDescription]
baseTypes =
  [imm, cls]

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
