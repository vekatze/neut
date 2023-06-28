module Entity.BaseName
  ( BaseName,
    bySplit,
    reify,
    reflect,
    reflect',
    hole,
    form,
    sigmaName,
    lambdaName,
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
    nat,
    natZero,
    natSucc,
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

nat :: BaseName
nat =
  MakeBaseName "nat"

natZero :: BaseName
natZero =
  MakeBaseName "Zero"

natSucc :: BaseName
natSucc =
  MakeBaseName "Succ"

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
    ("core.both", coreBothNames),
    ("core.cell", coreCellNames),
    ("core.channel", coreChannelNames),
    ("core.either", coreEitherNames),
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

coreBothNames :: [BaseName]
coreBothNames =
  map MakeBaseName ["both", "Both"]

coreCellNames :: [BaseName]
coreCellNames =
  map MakeBaseName ["cell", "new-cell", "mutate", "borrow", "clone"]

coreChannelNames :: [BaseName]
coreChannelNames =
  map MakeBaseName ["channel", "new-channel", "send", "receive"]

coreEitherNames :: [BaseName]
coreEitherNames =
  map MakeBaseName ["either", "Left", "Right", "option", "none-internal", "some-internal"]

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
  map MakeBaseName ["flow-inner", "detach", "attach"]

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
