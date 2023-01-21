module Entity.DefiniteDescription
  ( DefiniteDescription (..),
    new,
    newByGlobalLocator,
    newByDefiniteLocator,
    extend,
    extendLL,
    getFormDD,
    imm,
    cls,
    cell,
    array,
    isBaseDefiniteDescription,
    toBuilder,
  )
where

import Context.Throw qualified as Throw
import Data.Binary
import Data.ByteString.Builder
import Data.Hashable
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.DefiniteLocator qualified as DL
import Entity.Hint qualified as H
import Entity.LocalLocator qualified as LL
import Entity.ModuleID qualified as MID
import Entity.PrimType qualified as PT
import Entity.Section qualified as Section
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
  dd1 == dd2 = reify dd1 == reify dd2

instance Ord DefiniteDescription where
  compare dd1 dd2 = compare (reify dd1) (reify dd2)

instance Binary DefiniteDescription

instance Hashable DefiniteDescription

new :: SGL.StrictGlobalLocator -> LL.LocalLocator -> DefiniteDescription
new gl ll =
  MakeDefiniteDescription
    { globalLocator = gl,
      localLocator = ll,
      reify = SGL.reify gl <> definiteSep <> LL.reify ll
    }

newByGlobalLocator :: SGL.StrictGlobalLocator -> [Section.Section] -> BN.BaseName -> DefiniteDescription
newByGlobalLocator gl sectionStack name = do
  new gl $ LL.new sectionStack name

newByDefiniteLocator :: DL.DefiniteLocator -> LL.LocalLocator -> DefiniteDescription
newByDefiniteLocator dl ll = do
  let gl = DL.globalLocator dl
  let ll' = LL.new (LL.sectionStack ll ++ DL.sectionStack dl) (LL.baseName ll)
  new gl ll'

extend :: Throw.Context m => H.Hint -> DefiniteDescription -> T.Text -> m DefiniteDescription
extend m dd newName = do
  let gl = globalLocator dd
  let outer = localLocator dd
  inner <- LL.reflect m newName
  let ll = LL.extend outer inner
  return $ new gl ll

extendLL :: DefiniteDescription -> LL.LocalLocator -> DefiniteDescription
extendLL dd inner = do
  let gl = globalLocator dd
  let outer = localLocator dd
  new gl $ LL.extend outer inner

{-# INLINE toLowName #-}
toLowName :: DefiniteDescription -> T.Text
toLowName dd =
  wrapWithQuote $ reify dd

{-# INLINE wrapWithQuote #-}
wrapWithQuote :: T.Text -> T.Text
wrapWithQuote x =
  "\"" <> x <> "\""

-- this.core::nat.succ
-- ~> this.core::nat.succ.#.cons
getFormDD :: DefiniteDescription -> DefiniteDescription
getFormDD dd =
  extendLL dd $ LL.new Section.dummySectionStack BN.form

imm :: DefiniteDescription
imm =
  newByGlobalLocator (SGL.baseGlobalLocatorOf SL.internalLocator) [] BN.imm

cls :: DefiniteDescription
cls =
  newByGlobalLocator (SGL.baseGlobalLocatorOf SL.internalLocator) [] BN.cls

cell :: DefiniteDescription
cell =
  newByGlobalLocator (SGL.baseGlobalLocatorOf SL.internalLocator) [] BN.cell

array :: PT.PrimType -> DefiniteDescription
array elemType =
  newByGlobalLocator (SGL.baseGlobalLocatorOf SL.internalLocator) [] $ BN.arrayType elemType

isBaseDefiniteDescription :: DefiniteDescription -> Bool
isBaseDefiniteDescription dd =
  SGL.moduleID (globalLocator dd) == MID.Base

toBuilder :: DefiniteDescription -> Builder
toBuilder dd =
  TE.encodeUtf8Builder $ toLowName dd
