module Entity.EnumCase where

import Control.Comonad.Cofree
import Data.Binary
import Data.Functor.Classes
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import qualified Entity.EnumInfo as EI
import qualified Entity.EnumTypeName as ET
import qualified Entity.EnumValueName as EV
import Entity.Hint
import qualified Entity.UnresolvedName as UN
import GHC.Generics

data EnumLabel
  = EnumLabel ET.EnumTypeName D.Discriminant EV.EnumValueName
  deriving (Show, Eq, Ord, Generic)

instance Binary EnumLabel

-- fixme: prefer `newtype PreEnumLabel = PreEnumLabel UN.UnresolvedName`
data PreEnumLabel
  = PreEnumLabel UN.UnresolvedName D.Discriminant UN.UnresolvedName
  deriving (Show, Eq, Ord, Generic)

instance Binary PreEnumLabel

data EnumCaseF e a
  = EnumCaseLabel e
  | EnumCaseInt Integer
  | EnumCaseDefault
  deriving (Show, Eq, Ord, Generic)

-- data EnumCaseF e a
--   = EnumCaseLabel (e, Discriminant) EV.EnumValueName
--   | EnumCaseInt Integer
--   | EnumCaseDefault
--   deriving (Show, Eq, Ord, Generic)

instance Functor (EnumCaseF e) where
  fmap _ v =
    case v of
      EnumCaseLabel label ->
        EnumCaseLabel label
      EnumCaseInt i ->
        EnumCaseInt i
      EnumCaseDefault ->
        EnumCaseDefault

instance Eq e => Eq1 (EnumCaseF e) where
  liftEq _ v1 v2 =
    case (v1, v2) of
      (EnumCaseLabel l1, EnumCaseLabel l2)
        | l1 == l2 ->
          True
      (EnumCaseInt i1, EnumCaseInt i2)
        | i1 == i2 ->
          True
      (EnumCaseDefault, EnumCaseDefault) ->
        False
      _ ->
        False

instance Show e => Show1 (EnumCaseF e) where
  liftShowsPrec _ _ _ someValue =
    case someValue of
      EnumCaseLabel label ->
        showString $ show label
      EnumCaseInt i ->
        showString $ show i
      EnumCaseDefault ->
        showString "default"

instance (Binary a, Binary e) => Binary (EnumCaseF e a)

type PreEnumCase =
  Cofree (EnumCaseF PreEnumLabel) Hint

instance Binary PreEnumCase

type EnumCase =
  Cofree (EnumCaseF EnumLabel) Hint

type CompEnumCase =
  Cofree (EnumCaseF EnumLabel) ()

instance Binary EnumCase

weakenEnumLabel :: EnumLabel -> PreEnumLabel
weakenEnumLabel (EnumLabel enumTypeName discriminant enumValueName) =
  PreEnumLabel
    (UN.UnresolvedName $ DD.reify $ ET.reify enumTypeName)
    discriminant
    (UN.UnresolvedName $ DD.reify $ EV.reify enumValueName)

enumLabelTopUnit :: EnumLabel
enumLabelTopUnit =
  EnumLabel EI.constTop D.zero EI.constTopUnit

enumLabelBoolTrue :: EnumLabel
enumLabelBoolTrue =
  EnumLabel EI.constBool (D.increment D.zero) EI.constBoolTrue

enumLabelBoolFalse :: EnumLabel
enumLabelBoolFalse =
  EnumLabel EI.constBool D.zero EI.constBoolFalse
