module Entity.BaseName
  ( BaseName,
    bySplit,
    reify,
    reflect,
    reflect',
    length,
    hole,
    form,
    sigmaName,
    lambdaName,
    muName,
    textName,
    main,
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
    defaultImports,
  )
where

import Data.Binary
import Data.Hashable
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Const hiding (core)
import Entity.Error
import Entity.Hint qualified as H
import Entity.PrimType qualified as PT
import Entity.PrimType.ToText qualified as PT
import GHC.Generics
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
    else Left (newError m $ "no succeeding dots are allowed here: " <> name)

reflect :: H.Hint -> T.Text -> Either Error BaseName
reflect m rawTxt = do
  case map MakeBaseName $ T.split (nsSepChar ==) rawTxt of
    [baseName] ->
      return baseName
    _ ->
      Left $ newError m $ "no dots are allowed here: " <> rawTxt

reflect' :: T.Text -> Either Error BaseName
reflect' rawTxt = do
  case map MakeBaseName $ T.split (nsSepChar ==) rawTxt of
    [baseName] ->
      return baseName
    _ ->
      Left $ newError' $ "no dots are allowed here: " <> rawTxt

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

muName :: Int -> BaseName
muName i =
  MakeBaseName $ "mu;" <> T.pack (show i)

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

defaultImports :: [(T.Text, [BaseName])]
defaultImports =
  [ ("core.bool", coreBoolNames),
    ("core.pair", corePairNames),
    ("core.cell", coreCellNames),
    ("core.channel", coreChannelNames),
    ("core.except", coreExceptNames),
    ("core.file", coreFileNames),
    ("core.file.descriptor", coreFileDescriptorNames),
    ("core.function", coreFunctionNames),
    ("core.list", coreListNames),
    ("core.system", coreSystemNames),
    ("core.text", coreTextNames),
    ("core.text.io", coreTextIONames),
    ("core.thread", coreThreadNames),
    ("core.unit", coreUnitNames),
    ("core.void", coreVoidNames)
  ]

coreBoolNames :: [BaseName]
coreBoolNames =
  map MakeBaseName ["bool", "True", "False", "and", "or", "not"]

corePairNames :: [BaseName]
corePairNames =
  map MakeBaseName ["pair", "Pair"]

coreCellNames :: [BaseName]
coreCellNames =
  map MakeBaseName ["cell", "new-cell", "mutate", "borrow", "clone"]

coreChannelNames :: [BaseName]
coreChannelNames =
  map MakeBaseName ["channel", "new-channel", "send", "receive"]

coreExceptNames :: [BaseName]
coreExceptNames =
  map MakeBaseName ["except", "Fail", "Pass", "none"]

coreFileNames :: [BaseName]
coreFileNames =
  map MakeBaseName ["open", "close"]

coreFileDescriptorNames :: [BaseName]
coreFileDescriptorNames =
  map MakeBaseName ["descriptor", "stdin", "stdout", "stderr"]

coreFunctionNames :: [BaseName]
coreFunctionNames =
  map MakeBaseName ["flip", "compose", "curry", "uncurry"]

coreSystemNames :: [BaseName]
coreSystemNames =
  map MakeBaseName ["admit", "assert", "get-argc", "get-argv"]

coreTextNames :: [BaseName]
coreTextNames =
  [MakeBaseName "text"]

coreTextIONames :: [BaseName]
coreTextIONames =
  map MakeBaseName ["write", "read", "get-line", "print", "print-line", "print-int", "print-float"]

coreThreadNames :: [BaseName]
coreThreadNames =
  map MakeBaseName ["flow", "detach", "attach"]

coreUnitNames :: [BaseName]
coreUnitNames =
  map MakeBaseName ["unit", "Unit"]

coreVoidNames :: [BaseName]
coreVoidNames =
  [MakeBaseName "void"]

coreListNames :: [BaseName]
coreListNames = do
  map
    MakeBaseName
    [ "list",
      "Nil",
      "Cons",
      "length",
      "append",
      "fold-left",
      "fold-right",
      "map",
      "for",
      "concat",
      "reverse",
      "unzip",
      "uncons",
      "all",
      "any",
      "range",
      "filter-some"
    ]
