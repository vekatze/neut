module Entity.BaseName
  ( BaseName,
    bySplit,
    reify,
    reflect,
    reflect',
    bottom,
    top,
    topUnit,
    bool,
    boolTrue,
    boolFalse,
    sigmaName,
    lambdaName,
    main,
    fromText,
    this,
    new,
    base,
  )
where

-- import Entity.BaseName.Internal

import qualified Context.Throw as Throw
import Data.Binary
import Data.Hashable
import qualified Data.Text as T
import Entity.Const
import qualified Entity.Hint as H
import GHC.Generics

newtype BaseName = MakeBaseName {reify :: T.Text}
  deriving (Generic, Show, Eq)

instance Binary BaseName

instance Hashable BaseName

bySplit :: Throw.Context -> H.Hint -> T.Text -> IO [BaseName]
bySplit ctx m name = do
  let cand = map MakeBaseName $ T.split (nsSepChar ==) name
  if empty `notElem` cand
    then return $ map MakeBaseName $ T.split (nsSepChar ==) name
    else Throw.raiseError ctx m $ "invalid signature: " <> name

reflect :: Throw.Context -> H.Hint -> T.Text -> IO BaseName
reflect ctx m rawTxt = do
  case map MakeBaseName $ T.split (nsSepChar ==) rawTxt of
    [baseName] ->
      return baseName
    _ ->
      Throw.raiseError ctx m $ "invalid signature: " <> rawTxt

reflect' :: Throw.Context -> T.Text -> IO BaseName
reflect' ctx rawTxt = do
  case map MakeBaseName $ T.split (nsSepChar ==) rawTxt of
    [baseName] ->
      return baseName
    _ ->
      Throw.raiseError' ctx $ "invalid signature: " <> rawTxt

empty :: BaseName
empty =
  MakeBaseName ""

bottom :: BaseName
bottom =
  MakeBaseName "bottom"

top :: BaseName
top =
  MakeBaseName "top"

topUnit :: BaseName
topUnit =
  MakeBaseName "top"

bool :: BaseName
bool =
  MakeBaseName "bool"

boolTrue :: BaseName
boolTrue =
  MakeBaseName "true"

boolFalse :: BaseName
boolFalse =
  MakeBaseName "false"

this :: BaseName
this =
  MakeBaseName "this"

base :: BaseName
base =
  MakeBaseName "base"

main :: BaseName
main =
  MakeBaseName "main"

new :: BaseName
new =
  MakeBaseName "new"

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
