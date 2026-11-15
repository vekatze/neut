module Language.LowComp.Subst
  ( substOp,
    substLowValue,
    SubstLowComp,
  )
where

import Data.IntMap qualified as IntMap
import Language.Common.Ident.Reify
import Language.LowComp.LowComp qualified as LC

type SubstLowComp =
  IntMap.IntMap LC.Value

substOp :: SubstLowComp -> LC.Op -> LC.Op
substOp sub =
  LC.mapOpValues (substLowValue sub)

substLowValue :: SubstLowComp -> LC.Value -> LC.Value
substLowValue sub llvmValue =
  case llvmValue of
    LC.VarLocal x ->
      case IntMap.lookup (toInt x) sub of
        Just d ->
          d
        Nothing ->
          LC.VarLocal x
    _ ->
      llvmValue
