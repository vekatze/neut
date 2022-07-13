module Entity.BaseName
  ( BaseName,
    bySplit,
    reify,
    bottom,
    top,
    bool,
    sigmaName,
    lambdaName,
    main,
    fromText,
  )
where

-- import Entity.BaseName.Internal

import Data.Binary
import Data.Hashable
import qualified Data.Text as T
import Entity.Const
import GHC.Generics

newtype BaseName = MakeBaseName {reify :: T.Text}
  deriving (Generic, Show, Eq)

instance Binary BaseName

instance Hashable BaseName

bySplit :: T.Text -> [BaseName]
bySplit name = do
  map MakeBaseName $ T.split (nsSepChar ==) name

bottom :: BaseName
bottom =
  MakeBaseName "bottom"

top :: BaseName
top =
  MakeBaseName "top"

bool :: BaseName
bool =
  MakeBaseName "bool"

main :: BaseName
main =
  MakeBaseName "main"

sigmaName :: Int -> BaseName
sigmaName i =
  MakeBaseName $ "sigma;" <> T.pack (show i)

lambdaName :: Int -> BaseName
lambdaName i =
  MakeBaseName $ "lambda;" <> T.pack (show i)

{-# INLINE fromText #-}
fromText :: T.Text -> BaseName
fromText txt =
  case T.find (nsSepChar ==) txt of
    Nothing ->
      MakeBaseName txt
    Just _ ->
      error $
        "Entity.BaseName.fromText: given text `"
          <> T.unpack txt
          <> "` contains '.'"
