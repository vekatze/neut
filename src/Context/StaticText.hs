module Context.StaticText
  ( insert,
    getAll,
    StaticTextInfo,
  )
where

import Context.App
import Context.App.Internal
import Data.ByteString.Builder
import Entity.DefiniteDescription qualified as DD
import Prelude hiding (lookup, read)

type StaticTextInfo = (DD.DefiniteDescription, (Builder, Int))

insert :: DD.DefiniteDescription -> Builder -> Int -> App ()
insert name text len =
  modifyRef' staticTextList $ (:) (name, (text, len))

getAll :: App [StaticTextInfo]
getAll = do
  readRef' staticTextList
