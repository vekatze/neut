module Language.Comp.Comp
  ( Value (..),
    ForceInline,
    Comp (..),
    renderComp,
    Primitive (..),
    CompStmt (..),
    SubstValue,
    IsReducible,
    DefMap,
    toDefMap,
    Label,
    fromDefTuple,
    fromCompStmt,
    getCompStmtName,
    intValue0,
    intValue1,
    mulInt64,
    sigmaIntro,
    sigmaElim,
    isUnreachable,
    null,
  )
where

import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Language.Common.ArgNum
import Language.Common.BaseLowType
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Foreign qualified as F
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident
import Language.Common.Ident.Reify
import Language.Common.LowMagic
import Language.Common.Opacity
import Language.Common.PrimNumSize
import Language.Common.PrimOp
import Language.Common.PrimOp.BinaryOp qualified as BOp
import Language.Common.PrimType qualified as PT
import Language.Comp.EnumCase hiding (Int)
import Prelude hiding (null)

data Value
  = VarLocal Ident
  | VarGlobal DD.DefiniteDescription ArgNum (FCT.ForeignCodType BaseLowType)
  | VarStaticBytes BS.ByteString
  | SigmaIntro Int [Value]
  | Int IntSize Integer
  | Float FloatSize Double
  deriving (Eq)

instance Show Value where
  show v =
    case v of
      VarLocal x ->
        T.unpack $ toText' x
      VarGlobal dd _ _ ->
        T.unpack $ DD.reify dd
      VarStaticBytes bytes ->
        show $ BS.unpack bytes
      SigmaIntro size vs ->
        T.unpack $ renderValueText (SigmaIntro size vs)
      Language.Comp.Comp.Int _ i ->
        show i
      Language.Comp.Comp.Float _ f ->
        show f

type IsReducible = Bool

type ForceInline = Bool

type Label =
  Ident

data Comp
  = PiElimDownElim ForceInline Value [Value] -- ((force v) v1 ... vn)
  | SigmaElim ShouldDeallocate Int Int [Ident] Value Comp -- offset, allocation slot count
  | UpIntro Value
  | UpIntroVoid
  | UpElim IsReducible Ident Comp Comp
  | UpElimCallVoid Value [Value] Comp
  | EnumElim [(Int, Value)] Value Comp [(EnumCase, Comp)]
  | DestCall Comp Value [Value]
  | WriteToDest Value Comp Comp Comp
  | Primitive Primitive
  | Free Value (Maybe Int) Comp -- number of bytes to deallocate
  | Unreachable

instance Show Comp where
  show c =
    T.unpack $ renderComp 0 c

renderComp :: Int -> Comp -> T.Text
renderComp level comp =
  TL.toStrict $ B.toLazyText $ renderCompBuilder level comp

renderCompBuilder :: Int -> Comp -> B.Builder
renderCompBuilder level comp =
  case comp of
    PiElimDownElim forceInline v vs ->
      indent level <> btext (if forceInline then "force " else "") <> bshow v <> btext "@("
        <> bintercalate (map bshow vs) <> btext ")"
    SigmaElim shouldDeallocate offset size xs v cont ->
      indent level <> btext (if shouldDeallocate then "let" else "let-noetic") <> btext "<"
        <> bshow offset <> btext ", " <> bshow size <> btext "> ("
        <> bintercalate (map bshow xs) <> btext ") = " <> bshow v <> btext "\n"
        <> renderCompBuilder (continuationLevel level) cont
    UpIntro v ->
      indent level <> btext "return " <> bshow v
    UpIntroVoid ->
      indent level <> btext "return-void"
    UpElim isReducible x c1 c2 ->
      renderCompBinding
        level
        (btext "let" <> btext (if isReducible then "" else "*") <> btext " " <> bshow x <> btext " =")
        c1
        c2
    UpElimCallVoid f vs cont ->
      indent level <> btext "call-void " <> bshow f <> btext "@("
        <> bintercalate (map bshow vs) <> btext ")\n"
        <> renderCompBuilder (continuationLevel level) cont
    EnumElim substitution v defaultBranch caseList ->
      indent level <> btext "switch " <> bshow substitution <> btext " " <> bshow v <> btext " {"
        <> mconcat (map (renderEnumCase level) caseList) <> btext "\n" <> indent level
        <> btext "| default =>\n" <> renderCompBuilder (level + 1) defaultBranch
        <> btext "\n" <> indent level <> btext "}"
    DestCall sizeComp f vs ->
      indent level <> btext "dest-call " <> bshow f <> btext "@("
        <> bintercalate (map bshow vs) <> btext ") {\n" <> indent (level + 1)
        <> btext "size-comp:\n" <> renderCompBuilder (level + 2) sizeComp
        <> btext "\n" <> indent level <> btext "}"
    WriteToDest dest sizeComp result cont ->
      indent level <> btext "write-to-dest " <> bshow dest <> btext " {\n"
        <> renderCompSection (level + 1) "size-comp" sizeComp
        <> renderCompSection (level + 1) "result" result
        <> renderCompSection (level + 1) "cont" cont <> indent level <> btext "}"
    Primitive prim ->
      indent level <> btext "(" <> bshow prim <> btext ")"
    Free x size cont ->
      indent level <> btext "free(" <> bshow x <> btext ", " <> bshow size <> btext ")\n"
        <> renderCompBuilder (continuationLevel level) cont
    Unreachable ->
      indent level <> btext "⊥"

renderCompBinding :: Int -> B.Builder -> Comp -> Comp -> B.Builder
renderCompBinding level header value cont =
  case renderCompInline value of
    Just valueText ->
      indent level <> header <> btext " " <> valueText <> btext "\n"
        <> renderCompBuilder (continuationLevel level) cont
    Nothing ->
      indent level <> header <> btext "\n" <> renderCompBuilder (level + 1) value
        <> btext "\n" <> renderCompBuilder (continuationLevel level) cont

renderCompInline :: Comp -> Maybe B.Builder
renderCompInline comp =
  case comp of
    PiElimDownElim forceInline v vs ->
      Just $ btext (if forceInline then "force " else "") <> bshow v <> btext "@("
        <> bintercalate (map bshow vs) <> btext ")"
    UpIntro v ->
      Just $ btext "return " <> bshow v
    UpIntroVoid ->
      Just $ btext "return-void"
    Primitive prim ->
      Just $ btext "(" <> bshow prim <> btext ")"
    Unreachable ->
      Just $ btext "⊥"
    _ ->
      Nothing

renderEnumCase :: Int -> (EnumCase, Comp) -> B.Builder
renderEnumCase level (enumCase, comp) =
  btext "\n" <> indent level <> btext "| <" <> bshow enumCase <> btext "> =>\n"
    <> renderCompBuilder (level + 1) comp

renderCompSection :: Int -> String -> Comp -> B.Builder
renderCompSection level label comp =
  btext "\n" <> indent level <> btext label <> btext ":\n"
    <> renderCompBuilder (level + 1) comp

indent :: Int -> B.Builder
indent level =
  B.fromText $ T.replicate (level * 2) " "

btext :: String -> B.Builder
btext =
  B.fromString

bshow :: Show a => a -> B.Builder
bshow =
  B.fromString . show

bintercalate :: [B.Builder] -> B.Builder
bintercalate =
  mconcat . intersperse (btext ", ")

renderValueText :: Value -> T.Text
renderValueText value =
  TL.toStrict $ B.toLazyText $ renderValueBuilder value

renderValueBuilder :: Value -> B.Builder
renderValueBuilder value =
  case value of
    VarLocal x ->
      B.fromText $ toText' x
    VarGlobal dd _ _ ->
      B.fromText $ DD.reify dd
    VarStaticBytes bytes ->
      B.fromString $ show $ BS.unpack bytes
    SigmaIntro size vs ->
      B.fromString "[" <> B.fromString (show size) <> B.fromString "](" <> mconcat (intersperse (B.fromString ", ") (map renderValueBuilder vs)) <> B.fromString ")"
    Language.Comp.Comp.Int _ i ->
      B.fromString $ show i
    Language.Comp.Comp.Float _ f ->
      B.fromString $ show f

continuationLevel :: Int -> Int
continuationLevel level =
  if level == 0 then 1 else level

type ShouldDeallocate = Bool

data Primitive
  = PrimOp PrimOp [Value]
  | ShiftPointer Value Integer Integer -- (ptr, num-of-elems, index)
  | Calloc Value Value -- num, size-in-bytes
  | Alloc Value -- number of bytes to allocate
  | Realloc Value Value -- ptr, size-in-bytes
  | Memcpy Value Value Value -- (dest, src, size-in-bytes)
  | Magic (LowMagic BaseLowType Value Value)
  deriving (Show)

type SubstValue =
  IntMap.IntMap Value

data CompStmt
  = Def DD.DefiniteDescription Opacity [Ident] Comp
  | DefVoid DD.DefiniteDescription Opacity [Ident] Comp
  | Foreign [F.Foreign]

fromCompStmt :: CompStmt -> Maybe (Opacity, [Ident], Comp)
fromCompStmt cs =
  case cs of
    Def _ opacity xs body ->
      Just (opacity, xs, body)
    DefVoid _ opacity xs body ->
      Just (opacity, xs, body)
    Foreign {} ->
      Nothing

getCompStmtName :: CompStmt -> Maybe DD.DefiniteDescription
getCompStmtName stmt =
  case stmt of
    Def name _ _ _ ->
      Just name
    DefVoid name _ _ _ ->
      Just name
    Foreign {} ->
      Nothing

fromDefTuple :: (DD.DefiniteDescription, (Opacity, [Ident], Comp)) -> CompStmt
fromDefTuple (dd, (opacity, args, body)) =
  Def dd opacity args body

type DefMap =
  Map.HashMap DD.DefiniteDescription (Opacity, [Ident], Comp)

toDefMap :: [CompStmt] -> DefMap
toDefMap stmtList =
  Map.fromList $
    mapMaybe (\stmt -> (,) <$> getCompStmtName stmt <*> fromCompStmt stmt) stmtList

intValue0 :: Value
intValue0 =
  Int IntSize1 0

intValue1 :: Value
intValue1 =
  Int IntSize1 1

mulInt64 :: Value -> Value -> Primitive
mulInt64 x y =
  PrimOp (PrimBinaryOp BOp.Mul (PT.Int IntSize64) (PT.Int IntSize64)) [x, y]

sigmaIntro :: [Value] -> Value
sigmaIntro vs =
  SigmaIntro (length vs) vs

sigmaElim :: ShouldDeallocate -> [Ident] -> Value -> Comp -> Comp
sigmaElim shouldDeallocate xs =
  SigmaElim shouldDeallocate 0 (length xs) xs

isUnreachable :: Comp -> Bool
isUnreachable comp =
  case comp of
    PiElimDownElim {} ->
      False
    SigmaElim _ _ _ _ _ cont ->
      isUnreachable cont
    UpIntro {} ->
      False
    UpIntroVoid ->
      False
    UpElim _ _ e1 e2 ->
      isUnreachable e1 || isUnreachable e2
    UpElimCallVoid _ _ e ->
      isUnreachable e
    EnumElim _ _ defaultBranch branchList ->
      isUnreachable defaultBranch && all (isUnreachable . snd) branchList
    DestCall {} ->
      False
    WriteToDest _ _ result cont ->
      isUnreachable result || isUnreachable cont
    Primitive {} ->
      False
    Free _ _ cont ->
      isUnreachable cont
    Unreachable ->
      True

null :: Value
null =
  sigmaIntro []
