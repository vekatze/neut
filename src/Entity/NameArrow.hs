module Entity.NameArrow
  ( NameArrowF (..),
    RawNameArrow,
    NameArrow,
    InnerRawNameArrow,
    InnerNameArrow,
    getAllDomNames,
  )
where

import Data.Binary
import Entity.DefiniteDescription qualified as DD
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.VarOrLocator
import GHC.Generics

data NameArrowF a
  = Function a
  | Variant
      a -- original name
      [a] -- arrows for constructors/destructors
      [DD.DefiniteDescription] -- a list of (possibly restricted) constructors
  deriving (Generic)

instance Binary a => Binary (NameArrowF a)

type RawNameArrow =
  NameArrowF InnerRawNameArrow

type NameArrow =
  NameArrowF InnerNameArrow

type InnerRawNameArrow =
  (NameArrowDom, RawNameArrowCod)

type InnerNameArrow =
  (NameArrowDom, NameArrowCod)

type NameArrowDom =
  (Hint, DD.DefiniteDescription)

type RawNameArrowCod =
  (Hint, VarOrLocator)

type NameArrowCod =
  (Hint, DD.DefiniteDescription, GN.GlobalName)

getAllDomNames :: [NameArrow] -> [DD.DefiniteDescription]
getAllDomNames clauseList =
  case clauseList of
    [] ->
      []
    Function ((_, alias), _) : rest ->
      alias : getAllDomNames rest
    Variant ((_, dataAlias), _) consClauseList _ : rest ->
      dataAlias : getAllDomNames (map Function consClauseList ++ rest)
