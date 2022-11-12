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
    internalBaseName,
    cons,
    imm,
    cls,
    cell,
    arrayType,
    malloc,
    free,
  )
where

import qualified Context.Throw as Throw
import Data.Binary
import Data.Hashable
import qualified Data.Text as T
import Entity.Const
import qualified Entity.Hint as H
import qualified Entity.PrimNumType as PNT
import qualified Entity.PrimNumType.ToText as PNT
import GHC.Generics

newtype BaseName = MakeBaseName {reify :: T.Text}
  deriving (Generic, Show, Eq, Ord)

instance Binary BaseName

instance Hashable BaseName

bySplit :: Throw.Context m => H.Hint -> T.Text -> m [BaseName]
bySplit m name = do
  let cand = map MakeBaseName $ T.split (nsSepChar ==) name
  if empty `notElem` cand
    then return $ map MakeBaseName $ T.split (nsSepChar ==) name
    else Throw.raiseError m $ "invalid signature: " <> name

reflect :: Throw.Context m => H.Hint -> T.Text -> m BaseName
reflect m rawTxt = do
  case map MakeBaseName $ T.split (nsSepChar ==) rawTxt of
    [baseName] ->
      return baseName
    _ ->
      Throw.raiseError m $ "invalid signature: " <> rawTxt

reflect' :: Throw.Context m => T.Text -> m BaseName
reflect' rawTxt = do
  case map MakeBaseName $ T.split (nsSepChar ==) rawTxt of
    [baseName] ->
      return baseName
    _ ->
      Throw.raiseError' $ "invalid signature: " <> rawTxt

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
  MakeBaseName "unit"

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

imm :: BaseName
imm =
  MakeBaseName "imm"

cls :: BaseName
cls =
  MakeBaseName "cls"

cell :: BaseName
cell =
  MakeBaseName "cell"

sigmaName :: Int -> BaseName
sigmaName i =
  MakeBaseName $ "sigma;" <> T.pack (show i)

lambdaName :: Int -> BaseName
lambdaName i =
  MakeBaseName $ "lambda;" <> T.pack (show i)

cons :: BaseName
cons =
  MakeBaseName "cons"

malloc :: BaseName
malloc =
  MakeBaseName "malloc"

free :: BaseName
free =
  MakeBaseName "free"

arrayType :: PNT.PrimNumType -> BaseName
arrayType elemType =
  MakeBaseName $ "unsafe-" <> PNT.toText elemType <> "-array-internal"

internalBaseName :: BaseName
internalBaseName =
  MakeBaseName "#"

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
