module Kernel.Elaborate.Constraint
  ( Constraint (..),
    MetaVarSet,
    SuspendedConstraint (..),
    showConstraint,
    showConstraints,
    showSuspendedConstraint,
    showSuspendedConstraints,
  )
where

import Data.Set qualified as S
import Data.Text qualified as T
import Language.Common.HoleID qualified as HID
import Language.WeakTerm.ToText qualified as ToText
import Language.WeakTerm.WeakTerm qualified as WT

data Constraint
  = Eq WT.WeakType WT.WeakType -- (expected-type, actual-type)
  | Actual WT.WeakType
  | Integer WT.WeakType

type MetaVarSet =
  S.Set HID.HoleID

newtype SuspendedConstraint
  = SuspendedConstraint (MetaVarSet, (Constraint, Constraint))

showConstraint :: Constraint -> T.Text
showConstraint constraint =
  case constraint of
    Eq expected actual ->
      ToText.toTextType expected <> " == " <> ToText.toTextType actual
    Actual t ->
      "actual " <> ToText.toTextType t
    Integer t ->
      "integer " <> ToText.toTextType t

showConstraints :: [Constraint] -> T.Text
showConstraints constraints =
  T.unlines $ zipWith formatConstraint [1 :: Int ..] constraints
  where
    formatConstraint i c =
      T.pack (show i) <> ". " <> showConstraint c

showSuspendedConstraint :: SuspendedConstraint -> T.Text
showSuspendedConstraint (SuspendedConstraint (metaVars, (c1, c2))) =
  let metaVarsText = showMetaVarSet metaVars
      c1Text = showConstraint c1
      c2Text = showConstraint c2
   in "suspended [" <> metaVarsText <> "] (" <> c1Text <> ", " <> c2Text <> ")"

showSuspendedConstraints :: [SuspendedConstraint] -> T.Text
showSuspendedConstraints suspendedConstraints =
  T.unlines $ zipWith formatSuspendedConstraint [1 :: Int ..] suspendedConstraints
  where
    formatSuspendedConstraint i sc =
      T.pack (show i) <> ". " <> showSuspendedConstraint sc

showMetaVarSet :: MetaVarSet -> T.Text
showMetaVarSet metaVars =
  if S.null metaVars
    then ""
    else T.intercalate ", " $ map (T.pack . show . HID.reify) $ S.toList metaVars
