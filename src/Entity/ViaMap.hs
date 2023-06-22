module Entity.ViaMap
  ( ViaMap,
    PreViaMap,
    encode,
    decode,
  )
where

import Data.HashMap.Strict qualified as Map
import Entity.DefiniteDescription qualified as DD
import Entity.RawIdent

type ViaMap =
  Map.HashMap DD.DefiniteDescription (Map.HashMap RawIdent DD.DefiniteDescription)

type PreViaMap =
  [(DD.DefiniteDescription, [(RawIdent, DD.DefiniteDescription)])]

encode :: ViaMap -> PreViaMap
encode =
  Map.toList . Map.map Map.toList

decode :: PreViaMap -> ViaMap
decode =
  Map.fromList . map (fmap Map.fromList)
