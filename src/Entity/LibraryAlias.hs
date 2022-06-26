module Entity.LibraryAlias where

import qualified Data.Text as T

newtype LibraryAlias = LibraryAlias {extract :: T.Text}
