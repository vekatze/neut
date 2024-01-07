module Context.SymLoc (insert) where

import Context.App
import Context.Remark (printNote')
import Control.Monad (unless)
import Data.Text qualified as T
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reify

insert :: Ident -> Loc -> Loc -> App ()
insert x startLoc endLoc = do
  unless (isHole x) $ do
    printNote' $ toText' x <> ": " <> T.pack (show startLoc) <> " .. " <> T.pack (show endLoc)
