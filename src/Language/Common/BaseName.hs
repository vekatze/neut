module Language.Common.BaseName
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
    immType,
    immNoema,
    immInt1,
    immInt2,
    immInt4,
    immInt8,
    immInt16,
    immInt32,
    immInt64,
    immFloat16,
    immFloat32,
    immFloat64,
    immRune,
    immPointer,
    immNull,
    cls,
    cell,
    arrayType,
    malloc,
    free,
    reservedAlias,
    extend,
    node,
    leaf,
    root,
  )
where

import App.Error
import Data.Binary
import Data.Char (isUpper)
import Data.Hashable
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics
import Language.Common.Const
import Language.Common.Ident (Ident)
import Language.Common.Ident.Reify (toText)
import Language.Common.PrimType qualified as PT
import Language.Common.PrimType.ToText qualified as PT
import Logger.Hint qualified as H
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

immType :: BaseName
immType =
  MakeBaseName "imm-type"

immNoema :: BaseName
immNoema =
  MakeBaseName "imm-noema"

immInt1 :: BaseName
immInt1 =
  MakeBaseName "imm-int1"

immInt2 :: BaseName
immInt2 =
  MakeBaseName "imm-int2"

immInt4 :: BaseName
immInt4 =
  MakeBaseName "imm-int4"

immInt8 :: BaseName
immInt8 =
  MakeBaseName "imm-int8"

immInt16 :: BaseName
immInt16 =
  MakeBaseName "imm-int16"

immInt32 :: BaseName
immInt32 =
  MakeBaseName "imm-int32"

immInt64 :: BaseName
immInt64 =
  MakeBaseName "imm-int64"

immFloat16 :: BaseName
immFloat16 =
  MakeBaseName "imm-float16"

immFloat32 :: BaseName
immFloat32 =
  MakeBaseName "imm-float32"

immFloat64 :: BaseName
immFloat64 =
  MakeBaseName "imm-float64"

immRune :: BaseName
immRune =
  MakeBaseName "imm-rune"

immPointer :: BaseName
immPointer =
  MakeBaseName "imm-pointer"

immNull :: BaseName
immNull =
  MakeBaseName "imm-null"

cls :: BaseName
cls =
  MakeBaseName "cls"

cell :: BaseName
cell =
  MakeBaseName "cell"

sigmaName :: Int -> BaseName
sigmaName i =
  MakeBaseName $ "sigma;" <> T.pack (show i)

lambdaName :: Maybe T.Text -> Int -> BaseName
lambdaName mName i =
  case mName of
    Just name ->
      MakeBaseName $ "lambda;" <> name <> ";" <> T.pack (show i)
    Nothing ->
      MakeBaseName $ "lambda;anon;" <> T.pack (show i)

muName :: Ident -> Int -> BaseName
muName x i =
  MakeBaseName $ "mu;" <> toText x <> ";" <> T.pack (show i)

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

node :: BaseName
node =
  MakeBaseName "node"

leaf :: BaseName
leaf =
  MakeBaseName "leaf"

root :: BaseName
root =
  MakeBaseName "root"

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
