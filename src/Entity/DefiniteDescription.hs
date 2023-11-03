module Entity.DefiniteDescription
  ( DefiniteDescription (..),
    new,
    localLocator,
    globalLocator,
    getLocatorPair,
    newByGlobalLocator,
    getFormDD,
    imm,
    cls,
    toBuilder,
    llvmGlobalLocator,
  )
where

import Data.Binary
import Data.ByteString.Builder
import Data.Hashable
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.Error
import Entity.GlobalLocator qualified as GL
import Entity.Hint qualified as H
import Entity.LocalLocator qualified as LL
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
import GHC.Generics

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

globalLocator :: DefiniteDescription -> T.Text
globalLocator dd = do
  let nameList = T.splitOn nsSep (reify dd)
  case initLast nameList of
    Just (xs, _) ->
      T.intercalate nsSep xs
    _ ->
      error "Entity.DefiniteDescription.globalLocator"

localLocator :: DefiniteDescription -> T.Text
localLocator dd = do
  let nameList = T.splitOn "." (reify dd)
  case initLast nameList of
    Just (_, result) ->
      result
    _ ->
      error "Entity.DefiniteDescription.localLocator"

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
      Left $ newError m "Entity.DefiniteDescription.getLocatorPair: empty variable name"
    Just ([], _) ->
      Left $ newError m $ "the symbol `" <> varText <> "` doesn't contain a global locator"
    Just (initElems, lastElem) -> do
      gl <- GL.reflect m $ T.intercalate "." initElems
      ll <- LL.reflect m lastElem
      return (gl, ll)

initLast :: [a] -> Maybe ([a], a)
initLast xs =
  case xs of
    [] ->
      Nothing
    [x] ->
      return ([], x)
    x : rest -> do
      (initElems, lastElem) <- initLast rest
      return (x : initElems, lastElem)

llvmGlobalLocator :: T.Text
llvmGlobalLocator =
  "base.llvm"
