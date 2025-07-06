module Kernel.Common.RuleHandle.Global.KeyArg
  ( Handle (..),
    ExpKey,
    ImpKey,
    _eqKeys,
    _showKeys,
    _showKeyList,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Kernel.Common.Module
import Language.Common.Const (holeVarPrefix)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.IsConstLike
import Language.RawTerm.Key
import Prelude hiding (lookup, read)

type ExpKey =
  Key

type ImpKey =
  Key

data Handle = Handle
  { _mainModule :: MainModule,
    _keyArgMapRef :: IORef (Map.HashMap DD.DefiniteDescription (IsConstLike, ([ImpKey], [ExpKey])))
  }

isHole :: Key -> Bool
isHole =
  T.isPrefixOf holeVarPrefix

_eqKeys :: [Key] -> [Key] -> Bool
_eqKeys ks1 ks2 =
  case (ks1, ks2) of
    ([], []) ->
      True
    (_ : _, []) ->
      False
    ([], _ : _) ->
      False
    (k1 : rest1, k2 : rest2) ->
      case (isHole k1, isHole k2) of
        (True, True) ->
          _eqKeys rest1 rest2
        (True, False) ->
          False
        (False, True) ->
          False
        (False, False)
          | k1 == k2 ->
              _eqKeys rest1 rest2
          | otherwise ->
              False

_showKeys :: [Key] -> T.Text
_showKeys keys =
  case keys of
    [] ->
      "(empty)"
    [k] ->
      showKey k
    k : rest ->
      showKey k <> ", " <> _showKeys rest

showKey :: Key -> T.Text
showKey k =
  if isHole k
    then "_"
    else k

_showKeyList :: [Key] -> T.Text
_showKeyList ks =
  T.intercalate "\n" $ map ("- " <>) ks
