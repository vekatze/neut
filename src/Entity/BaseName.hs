module Entity.BaseName
  ( BaseName,
    bySplit,
    reify,
    reflect,
    reflect',
    form,
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
    reservedAlias,
    extend,
  )
where

import Data.Binary
import Data.Hashable
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Const
import Entity.Hint qualified as H
import Entity.Log
import Entity.PrimType qualified as PT
import Entity.PrimType.ToText qualified as PT
import GHC.Generics

newtype BaseName = MakeBaseName {reify :: T.Text}
  deriving (Generic, Show, Eq, Ord)

instance Binary BaseName

instance Hashable BaseName

bySplit :: H.Hint -> T.Text -> Either Error [BaseName]
bySplit m name = do
  let cand = map MakeBaseName $ T.split (nsSepChar ==) name
  if empty `notElem` cand
    then return $ map MakeBaseName $ T.split (nsSepChar ==) name
    else Left (newError m $ "invalid signature: " <> name)

reflect :: H.Hint -> T.Text -> Either Error BaseName
reflect m rawTxt = do
  case map MakeBaseName $ T.split (nsSepChar ==) rawTxt of
    [baseName] ->
      return baseName
    _ ->
      Left $ newError m $ "invalid signature: " <> rawTxt

reflect' :: T.Text -> Either Error BaseName
reflect' rawTxt = do
  case map MakeBaseName $ T.split (nsSepChar ==) rawTxt of
    [baseName] ->
      return baseName
    _ ->
      Left $ newError' $ "invalid signature: " <> rawTxt

empty :: BaseName
empty =
  MakeBaseName ""

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
  MakeBaseName "New"

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

form :: BaseName
form =
  MakeBaseName "form"

malloc :: BaseName
malloc =
  MakeBaseName "malloc"

free :: BaseName
free =
  MakeBaseName "free"

arrayType :: PT.PrimType -> BaseName
arrayType elemType =
  MakeBaseName $ "unsafe-" <> PT.toText elemType <> "-array-internal"

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

reservedAlias :: S.Set BaseName
reservedAlias =
  S.fromList
    [ this,
      base
    ]

extend :: BaseName -> BaseName -> BaseName
extend (MakeBaseName b1) (MakeBaseName b2) =
  MakeBaseName $ b1 <> "#" <> b2
