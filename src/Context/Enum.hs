module Context.Enum
  ( insert,
    isMember,
  )
where

import Context.App
import Context.App.Internal
import Data.Set qualified as S
import Entity.DefiniteDescription qualified as DD

insert :: DD.DefiniteDescription -> App ()
insert dd = do
  modifyRef' enumSet $ S.insert dd

isMember :: DD.DefiniteDescription -> App Bool
isMember dd = do
  S.member dd <$> readRef' enumSet
