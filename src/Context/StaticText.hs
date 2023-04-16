module Context.StaticText
  ( insert,
    getAll,
    StaticTextInfo,
  )
where

import Context.App
import Context.App.Internal
import Data.Text qualified as T
import Entity.DefiniteDescription qualified as DD
import Prelude hiding (lookup, read)

type StaticTextInfo = (DD.DefiniteDescription, (T.Text, Int))

insert :: DD.DefiniteDescription -> T.Text -> Int -> App ()
insert name text len =
  modifyRef' staticTextList $ (:) (name, (text, len))

getAll :: App [StaticTextInfo]
getAll = do
  readRef' staticTextList
