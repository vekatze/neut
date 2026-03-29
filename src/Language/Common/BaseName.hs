module Language.Common.BaseName
  ( BaseName,
    bySplit,
    reify,
    reflect,
    reflect',
    isCapitalized,
    form,
    sigmaName,
    lambdaName,
    muName,
    resourceName,
    mainName,
    zenName,
    fromText,
    this,
    base,
    core,
    imm,
    cls,
    reservedAlias,
    node,
    leaf,
    root,
    list,
    nil,
    consName,
    typeValue,
    binary,
    vector,
    left,
    right,
    eitherType,
    unit,
    unitType,
    constructor,
    constructorType,
    pair,
    pairType,
    textType,
    boolType,
    trueConstructor,
    falseConstructor,
    fromTypeTag,
  )
where

import App.Error
import Data.Binary
import Data.Char (isUpper)
import Data.Hashable
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics
import Kernel.Common.TypeTag qualified as TT
import Language.Common.Const
import Language.Common.Ident (Ident)
import Language.Common.Ident.Reify (toText)
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

empty :: BaseName
empty =
  MakeBaseName ""

this :: BaseName
this =
  MakeBaseName "this"

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

imm :: BaseName
imm =
  MakeBaseName "imm"

cls :: BaseName
cls =
  MakeBaseName "cls"

sigmaName :: Maybe T.Text -> Int -> BaseName
sigmaName mName i =
  case mName of
    Just name ->
      MakeBaseName $ name <> ";" <> T.pack (show i)
    Nothing ->
      MakeBaseName $ "sigma;" <> T.pack (show i)

-- MakeBaseName $ "sigma;" <> T.pack (show i)

lambdaName :: Maybe T.Text -> Int -> BaseName
lambdaName mName i =
  case mName of
    Just name ->
      MakeBaseName $ name <> ";" <> T.pack (show i)
    Nothing ->
      MakeBaseName $ "lambda;" <> T.pack (show i)

muName :: Ident -> Int -> BaseName
muName x i =
  MakeBaseName $ toText x <> ";" <> T.pack (show i)

resourceName :: Int -> BaseName
resourceName i =
  MakeBaseName $ "resource;" <> T.pack (show i)

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

list :: BaseName
list =
  MakeBaseName "list"

nil :: BaseName
nil =
  MakeBaseName "Nil"

consName :: BaseName
consName =
  MakeBaseName "Cons"

typeValue :: BaseName
typeValue =
  MakeBaseName "type-value"

binary :: BaseName
binary =
  MakeBaseName "binary"

vector :: BaseName
vector =
  MakeBaseName "vector"

left :: BaseName
left =
  MakeBaseName "Left"

right :: BaseName
right =
  MakeBaseName "Right"

unit :: BaseName
unit =
  MakeBaseName "Unit"

unitType :: BaseName
unitType =
  MakeBaseName "unit"

constructor :: BaseName
constructor =
  MakeBaseName "Constructor"

constructorType :: BaseName
constructorType =
  MakeBaseName "constructor"

pair :: BaseName
pair =
  MakeBaseName "Pair"

pairType :: BaseName
pairType =
  MakeBaseName "pair"

eitherType :: BaseName
eitherType =
  MakeBaseName "either"

textType :: BaseName
textType =
  MakeBaseName "text"

boolType :: BaseName
boolType =
  MakeBaseName "bool"

trueConstructor :: BaseName
trueConstructor =
  MakeBaseName "True"

falseConstructor :: BaseName
falseConstructor =
  MakeBaseName "False"

fromTypeTag :: TT.TypeTag -> BaseName
fromTypeTag tt =
  case tt of
    TT.Opaque ->
      MakeBaseName "Opaque"
    TT.Type ->
      MakeBaseName "Type"
    TT.Function ->
      MakeBaseName "Function"
    TT.Algebraic ->
      MakeBaseName "Algebraic"
    TT.Noema ->
      MakeBaseName "Noema"
    TT.Enum ->
      MakeBaseName "Enum"
    TT.Int1 ->
      MakeBaseName "Int1"
    TT.Int2 ->
      MakeBaseName "Int2"
    TT.Int4 ->
      MakeBaseName "Int4"
    TT.Int8 ->
      MakeBaseName "Int8"
    TT.Int16 ->
      MakeBaseName "Int16"
    TT.Int32 ->
      MakeBaseName "Int32"
    TT.Int64 ->
      MakeBaseName "Int64"
    TT.Float16 ->
      MakeBaseName "Float16"
    TT.Float32 ->
      MakeBaseName "Float32"
    TT.Float64 ->
      MakeBaseName "Float64"
    TT.Pointer ->
      MakeBaseName "Pointer"
    TT.Null ->
      MakeBaseName "Null"
    TT.Rune ->
      MakeBaseName "Rune"
    TT.Binary ->
      MakeBaseName "Binary"
    TT.Vector ->
      MakeBaseName "Vector"
    TT.Wrapper ->
      MakeBaseName "Wrapper"
    TT.BoxT ->
      MakeBaseName "Box-T"

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
