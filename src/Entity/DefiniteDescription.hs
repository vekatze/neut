module Entity.DefiniteDescription
  ( DefiniteDescription (..),
    new,
    getLocatorPair,
    newByGlobalLocator,
    getLocalName,
    getFormDD,
    imm,
    cls,
    toBuilder,
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

data DefiniteDescription = MakeDefiniteDescription
  { globalLocator :: SGL.StrictGlobalLocator,
    localLocator :: LL.LocalLocator,
    reify :: T.Text -- cache
  }
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
    { globalLocator = gl,
      localLocator = ll,
      reify = SGL.reify gl <> nsSep <> LL.reify ll
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

getLocalName :: DefiniteDescription -> T.Text
getLocalName dd =
  LL.reify $ localLocator dd

-- this.core::nat.succ
-- ~> this.core::nat.succ.#.cons
getFormDD :: DefiniteDescription -> DefiniteDescription
getFormDD dd = do
  let gl = globalLocator dd
  let ll = LL.extend (localLocator dd) BN.form
  new gl ll

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
      globalLocator <- GL.reflect m $ T.intercalate "." initElems
      localLocator <- LL.reflect m lastElem
      return (globalLocator, localLocator)

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
