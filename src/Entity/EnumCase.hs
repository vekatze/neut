module Entity.EnumCase where

import Control.Comonad.Cofree
import Data.Binary
import Data.Functor.Classes
import qualified Data.Text as T
import Entity.Discriminant
import qualified Entity.EnumTypeName as ET
import Entity.Hint
import GHC.Generics

data EnumCaseF a
  = EnumCaseLabel (ET.EnumTypeName, Discriminant) T.Text
  | EnumCaseInt Integer
  | EnumCaseDefault
  deriving (Show, Eq, Ord, Generic)

instance Functor EnumCaseF where
  fmap _ v =
    case v of
      EnumCaseLabel labelInfo label ->
        EnumCaseLabel labelInfo label
      EnumCaseInt i ->
        EnumCaseInt i
      EnumCaseDefault ->
        EnumCaseDefault

instance Eq1 EnumCaseF where
  liftEq _ v1 v2 =
    case (v1, v2) of
      (EnumCaseLabel labelInfo1 l1, EnumCaseLabel labelInfo2 l2)
        | labelInfo1 == labelInfo2,
          l1 == l2 ->
          True
      (EnumCaseInt i1, EnumCaseInt i2)
        | i1 == i2 ->
          True
      (EnumCaseDefault, EnumCaseDefault) ->
        False
      _ ->
        False

instance Show1 EnumCaseF where
  liftShowsPrec _ _ _ someValue =
    case someValue of
      EnumCaseLabel _ label ->
        showString $ T.unpack label
      EnumCaseInt i ->
        showString $ show i
      EnumCaseDefault ->
        showString "default"

instance (Binary a) => Binary (EnumCaseF a)

type EnumCase =
  Cofree EnumCaseF Hint

type CompEnumCase =
  Cofree EnumCaseF ()

instance Binary EnumCase
