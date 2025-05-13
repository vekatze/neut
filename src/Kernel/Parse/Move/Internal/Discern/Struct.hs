module Kernel.Parse.Move.Internal.Discern.Struct
  ( ensureFieldLinearity,
  )
where

import Error.Move.Run (raiseError)
import Error.Rule.EIO (EIO)
import Logger.Rule.Hint
import Data.Set qualified as S
import Data.Text qualified as T
import Language.RawTerm.Rule.Key

ensureFieldLinearity ::
  Hint ->
  [Key] ->
  S.Set Key ->
  S.Set Key ->
  EIO ()
ensureFieldLinearity m ks found nonLinear =
  case ks of
    [] ->
      if S.null nonLinear
        then return ()
        else
          raiseError m $
            "The following fields are defined more than once:\n"
              <> T.intercalate "\n" (map ("- " <>) (S.toList nonLinear))
    k : rest -> do
      if S.member k found
        then ensureFieldLinearity m rest found (S.insert k nonLinear)
        else ensureFieldLinearity m rest (S.insert k found) nonLinear
