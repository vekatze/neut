module Language.Common.Rule.BaseName
  ( BaseName,
    bySplit,
    reify,
    reflect,
    reflect',
    isCapitalized,
    length,
    hole,
    form,
    sigmaName,
    lambdaName,
    muName,
    resourceName,
    textName,
    mainName,
    zenName,
    fromText,
    this,
    new,
    base,
    core,
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
import Data.Char (isUpper)
import Data.Hashable
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics
import Language.Common.Rule.Const
import Language.Common.Rule.PrimType qualified as PT
import Language.Common.Rule.PrimType.ToText qualified as PT
import Library.Error.Rule.Error
import Library.Logger.Rule.Hint qualified as H
import Prelude hiding (length)

newtype BaseName = MakeBaseName {reify :: T.Text}
  deriving (Generic, Show, Eq, Ord)

instance Binary BaseName

instance Hashable BaseName

bySplit :: H.Hint -> T.Text -> Either Error [BaseName]
bySplit m name = do
  let cand = map MakeBaseName $ T.split (nsSepChar ==) name
  if empty `notElem` cand
    then return $ map MakeBaseName $ T.split (nsSepChar ==) name
    else Left (newError m $ "No succeeding dots are allowed here: " <> name)

reflect :: H.Hint -> T.Text -> Either Error BaseName
reflect m rawTxt = do
  case map MakeBaseName $ T.split (nsSepChar ==) rawTxt of
    [baseName] ->
      return baseName
    _ ->
      Left $ newError m $ "No dots are allowed here: " <> rawTxt

reflect' :: T.Text -> Either Error BaseName
reflect' rawTxt = do
  case map MakeBaseName $ T.split (nsSepChar ==) rawTxt of
    [baseName] ->
      return baseName
    _ ->
      Left $ newError' $ "No dots are allowed here: " <> rawTxt

isCapitalized :: BaseName -> Bool
isCapitalized (MakeBaseName bn) =
  case T.uncons bn of
    Nothing ->
      False
    Just (c, _) ->
      isUpper c

length :: BaseName -> Int
length MakeBaseName {reify} =
  T.length reify

empty :: BaseName
empty =
  MakeBaseName ""

this :: BaseName
this =
  MakeBaseName "this"

hole :: BaseName
hole =
  MakeBaseName "_"

base :: BaseName
base =
  MakeBaseName "base"

core :: BaseName
core =
  MakeBaseName "core"

mainName :: BaseName
mainName =
  MakeBaseName "main"

zenName :: BaseName
zenName =
  MakeBaseName "zen"

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

muName :: Int -> BaseName
muName i =
  MakeBaseName $ "mu;" <> T.pack (show i)

resourceName :: Int -> BaseName
resourceName i =
  MakeBaseName $ "resource;" <> T.pack (show i)

textName :: Int -> BaseName
textName i =
  MakeBaseName $ "text;" <> T.pack (show i)

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
        "Rule.BaseName.fromText: given text `"
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
