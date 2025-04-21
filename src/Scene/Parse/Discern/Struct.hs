module Scene.Parse.Discern.Struct
  ( ensureFieldLinearity,
  )
where

import Context.App
import Context.Throw qualified as Throw
import Data.Set qualified as S
import Data.Text qualified as T
import Rule.Hint
import Rule.Key

ensureFieldLinearity ::
  Hint ->
  [Key] ->
  S.Set Key ->
  S.Set Key ->
  App ()
ensureFieldLinearity m ks found nonLinear =
  case ks of
    [] ->
      if S.null nonLinear
        then return ()
        else
          Throw.raiseError m $
            "The following fields are defined more than once:\n"
              <> T.intercalate "\n" (map ("- " <>) (S.toList nonLinear))
    k : rest -> do
      if S.member k found
        then ensureFieldLinearity m rest found (S.insert k nonLinear)
        else ensureFieldLinearity m rest (S.insert k found) nonLinear
