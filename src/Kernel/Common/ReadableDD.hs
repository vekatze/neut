module Kernel.Common.ReadableDD
  ( readableDD,
    readableDD',
  )
where

import Data.HashMap.Strict qualified as Map
import Data.List (find)
import Data.Text qualified as T
import Kernel.Common.Module hiding (moduleID)
import Language.Common.Const
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ModuleAlias qualified as MA
import Language.Common.ModuleID qualified as MID

readableDD :: MainModule -> DD.DefiniteDescription -> T.Text
readableDD mainModule = do
  readableDD' (extractModule mainModule)

readableDD' :: Module -> DD.DefiniteDescription -> T.Text
readableDD' baseModule dd = do
  case DD.unconsDD dd of
    (MID.Main, rest) ->
      "this" <> nsSep <> rest
    (MID.Base, rest) ->
      "base" <> nsSep <> rest
    (MID.Library digest, rest) -> do
      let depMap = Map.toList $ moduleDependency baseModule
      let aliasOrNone = fmap (MA.reify . fst) $ flip find depMap $ \(_, dependency) -> do
            digest == dependencyDigest dependency
      case aliasOrNone of
        Nothing ->
          DD.reify dd
        Just alias ->
          alias <> nsSep <> rest
