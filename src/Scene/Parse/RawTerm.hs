module Scene.Parse.RawTerm
  ( rawTerm,
    rawTermSimple,
    preBinder,
    preAscription,
    parseTopDefInfo,
    parseDefiniteDescription,
    preVar,
  )
where

import Codec.Binary.UTF8.String
import Context.App
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Trans
import Data.Bifunctor
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.Binder
import Entity.Const
import Entity.DefiniteDescription qualified as DD
import Entity.ExternalName qualified as EN
import Entity.GlobalLocator qualified as GL
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reflect qualified as Ident
import Entity.LamKind qualified as LK
import Entity.LocalLocator qualified as LL
import Entity.LowType qualified as LT
import Entity.Magic qualified as M
import Entity.Opacity qualified as O
import Entity.PrimNumSize qualified as PNS
import Entity.PrimType qualified as PT
import Entity.PrimType.FromText qualified as PT
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.TargetPlatform qualified as TP
import Entity.UnresolvedName qualified as UN
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Scene.Parse.Core
import Text.Megaparsec

--
-- parser for RT.RawTerm
--

rawTerm :: Parser RT.RawTerm
rawTerm = do
  m <- getCurrentHint
  e1 <- rawTermBasic
  choice
    [ rawTermSeq m e1,
      rawTermExplicitAscription m e1,
      return e1
    ]

rawTermBasic :: Parser RT.RawTerm
rawTermBasic = do
  choice
    [ rawTermPiIntro,
      rawTermPiIntroDef,
      rawTermListIntro,
      rawTermIntrospect,
      rawTermMagic,
      rawTermMatchNoetic,
      rawTermMatch,
      try rawTermNew,
      rawTermIf,
      rawTermLetCoproduct,
      try rawTermLetOn,
      rawTermLet,
      try rawTermPi,
      rawTermBasic'
    ]

rawTermBasic' :: Parser RT.RawTerm
rawTermBasic' = do
  choice
    [ rawTermNoema,
      rawTermEmbody,
      try rawTermPiElim,
      rawTermSimple
    ]

rawTermSimple :: Parser RT.RawTerm
rawTermSimple = do
  choice
    [ rawTermParen,
      rawTermTextIntro,
      rawTermTau,
      rawTermAdmit,
      rawTermHole,
      rawTermInteger,
      rawTermFloat,
      rawTermDefiniteDescription,
      rawTermVar
    ]

rawTermLetOn :: Parser RT.RawTerm
rawTermLetOn = do
  m <- getCurrentHint
  try $ keyword "let"
  x <- rawTermLetVar
  try $ keyword "on"
  noeticVarList <- map (second Ident.fromText) <$> commaList var
  delimiter "="
  e1 <- rawTerm
  keyword "in"
  e2 <- rawTerm
  return $ m :< RT.Let x noeticVarList e1 e2

rawTermLet :: Parser RT.RawTerm
rawTermLet = do
  m <- getCurrentHint
  try $ keyword "let"
  x <- rawTermLetVar
  delimiter "="
  e1 <- rawTerm
  keyword "in"
  e2 <- rawTerm
  return $ m :< RT.Let x [] e1 e2

-- let? x     = e1 in e2
rawTermLetCoproduct :: Parser RT.RawTerm
rawTermLetCoproduct = do
  m <- getCurrentHint
  try $ keyword "let?"
  x <- Ident.fromText <$> symbol
  delimiter "="
  e1 <- rawTerm
  keyword "in"
  e2 <- rawTerm
  err <- lift $ Gensym.newTextualIdentFromText "err"
  sumLeft <- lift $ handleDefiniteDescriptionIntoRawConsName m coreSumLeft
  sumRight <- lift $ handleDefiniteDescriptionIntoRawConsName m coreSumRight
  sumLeftVar <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreSumLeft
  return $
    m
      :< RT.DataElim
        False
        [e1]
        ( RP.new
            [ (V.fromList [(m, RP.Cons sumLeft [(m, RP.Var err)])], m :< RT.PiElim sumLeftVar [preVar' m err]),
              (V.fromList [(m, RP.Cons sumRight [(m, RP.Var x)])], e2)
            ]
        )

rawTermEmbody :: Parser RT.RawTerm
rawTermEmbody = do
  m <- getCurrentHint
  delimiter "*"
  e <- rawTermBasic'
  t <- lift $ Gensym.newPreHole m
  raw <- lift $ Gensym.newTextualIdentFromText "raw"
  copied <- lift $ Gensym.newTextualIdentFromText "copied"
  original <- lift $ Gensym.newTextualIdentFromText "original"
  return $
    bind (m, raw, t) (m :< RT.Magic (M.Cast (m :< RT.Noema t) t e)) $
      bind (m, original, m :< RT.Noema t) (m :< RT.Magic (M.Cast t (m :< RT.Noema t) (m :< RT.Var raw))) $
        bind (m, copied, t) (m :< RT.Var raw) $
          m :< RT.Var copied

rawTermSeq :: Hint -> RT.RawTerm -> Parser RT.RawTerm
rawTermSeq m e1 = do
  delimiter ";"
  e2 <- rawTerm
  f <- lift $ Gensym.newTextualIdentFromText "unit"
  top <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreTop
  return $ bind (m, f, m :< RT.PiElim top []) e1 e2

rawTermExplicitAscription :: Hint -> RT.RawTerm -> Parser RT.RawTerm
rawTermExplicitAscription m e = do
  delimiter ":"
  t <- rawTermBasic
  f <- lift $ Gensym.newTextualIdentFromText "unit"
  return $ bind (m, f, t) e (m :< RT.Var f)

rawTermTau :: Parser RT.RawTerm
rawTermTau = do
  m <- getCurrentHint
  try $ keyword "tau"
  return $ m :< RT.Tau

rawTermHole :: Parser RT.RawTerm
rawTermHole = do
  m <- getCurrentHint
  delimiter "?"
  lift $ Gensym.newPreHole m

rawTermPi :: Parser RT.RawTerm
rawTermPi = do
  m <- getCurrentHint
  domList <-
    choice
      [ argList $ choice [try preAscription, typeWithoutIdent],
        do
          x <- lift $ Gensym.newTextualIdentFromText "_"
          t <- rawTermBasic'
          return [(m, x, t)]
      ]
  delimiter "->"
  cod <- rawTerm
  return $ m :< RT.Pi domList cod

rawTermPiIntro :: Parser RT.RawTerm
rawTermPiIntro = do
  m <- getCurrentHint
  try $ keyword "lambda"
  varList <- argList preBinder
  e <- betweenBrace rawTerm
  return $ lam m varList e

parseDefInfo :: Hint -> Parser RT.DefInfo
parseDefInfo m = do
  functionVar <- var
  domInfoList <- argList preBinder
  codType <- parseDefInfoCod m
  e <- betweenBrace rawTerm
  return (functionVar, domInfoList, codType, e)

parseTopDefInfo :: Parser RT.TopDefInfo
parseTopDefInfo = do
  m <- getCurrentHint
  funcBaseName <- baseName
  impDomInfoList <- impArgList preBinder
  domInfoList <- argList preBinder
  codType <- parseDefInfoCod m
  e <- betweenBrace rawTerm
  return ((m, funcBaseName), impDomInfoList, domInfoList, codType, e)

parseDefInfoCod :: Hint -> Parser RT.RawTerm
parseDefInfoCod m =
  choice
    [ do
        delimiter ":"
        rawTerm,
      lift $ Gensym.newPreHole m
    ]

-- define name(x1: A1, ..., xn: An)[: A] as e end
rawTermPiIntroDef :: Parser RT.RawTerm
rawTermPiIntroDef = do
  m <- getCurrentHint
  try $ keyword "define"
  ((mFun, functionName), domBinderList, codType, e) <- parseDefInfo m
  let piType = mFun :< RT.Pi domBinderList codType
  return $ m :< RT.PiIntro (LK.Fix (mFun, Ident.fromText functionName, piType)) domBinderList e

parseDefiniteDescription :: Parser (Hint, GL.GlobalLocator, LL.LocalLocator)
parseDefiniteDescription = do
  m <- getCurrentHint
  globalLocator <- symbol
  globalLocator' <- lift $ Throw.liftEither $ GL.reflect m globalLocator
  delimiter definiteSep
  localLocator <- parseLocalLocator
  return (m, globalLocator', localLocator)

rawTermDefiniteDescription :: Parser RT.RawTerm
rawTermDefiniteDescription = do
  (m, globalLocator, localLocator) <- try parseDefiniteDescription
  return $ m :< RT.VarGlobal globalLocator localLocator

parseLocalLocator :: Parser LL.LocalLocator
parseLocalLocator = do
  m <- getCurrentHint
  rawTxt <- symbol
  lift $ Throw.liftEither $ LL.reflect m rawTxt

rawTermMagic :: Parser RT.RawTerm
rawTermMagic = do
  m <- getCurrentHint
  try $ keyword "magic"
  choice
    [ rawTermMagicCast m,
      rawTermMagicStore m,
      rawTermMagicLoad m,
      rawTermMagicSyscall m,
      rawTermMagicExternal m
    ]

rawTermMagicBase :: T.Text -> Parser a -> Parser a
rawTermMagicBase k parser = do
  keyword k
  betweenParen parser

rawTermMagicCast :: Hint -> Parser RT.RawTerm
rawTermMagicCast m = do
  rawTermMagicBase "cast" $ do
    castFrom <- rawTerm
    castTo <- delimiter "," >> rawTerm
    value <- delimiter "," >> rawTerm
    return $ m :< RT.Magic (M.Cast castFrom castTo value)

rawTermMagicStore :: Hint -> Parser RT.RawTerm
rawTermMagicStore m = do
  rawTermMagicBase "store" $ do
    lt <- lowType
    pointer <- delimiter "," >> rawTerm
    value <- delimiter "," >> rawTerm
    return $ m :< RT.Magic (M.Store lt pointer value)

rawTermMagicLoad :: Hint -> Parser RT.RawTerm
rawTermMagicLoad m = do
  rawTermMagicBase "load" $ do
    lt <- lowType
    pointer <- delimiter "," >> rawTerm
    return $ m :< RT.Magic (M.Load lt pointer)

rawTermMagicSyscall :: Hint -> Parser RT.RawTerm
rawTermMagicSyscall m = do
  rawTermMagicBase "syscall" $ do
    syscallNum <- integer
    es <- many (delimiter "," >> rawTerm)
    return $ m :< RT.Magic (M.Syscall syscallNum es)

rawTermMagicExternal :: Hint -> Parser RT.RawTerm
rawTermMagicExternal m = do
  rawTermMagicBase "external" $ do
    extFunName <- symbol
    es <- many (delimiter "," >> rawTerm)
    return $ m :< RT.Magic (M.External (EN.ExternalName extFunName) es)

-- -- t ::= i{n} | f{n} | pointer t | array INT t | struct t ... t
lowType :: Parser LT.LowType
lowType = do
  choice
    [ lowTypePointer,
      lowTypeArray,
      lowTypeStruct,
      lowTypeNumber
    ]

lowTypePointer :: Parser LT.LowType
lowTypePointer = do
  keyword "pointer"
  LT.Pointer <$> betweenParen lowType

lowTypeArray :: Parser LT.LowType
lowTypeArray = do
  keyword "array"
  betweenParen $ do
    intValue <- integer
    delimiter ","
    LT.Array (fromInteger intValue) <$> lowType

lowTypeStruct :: Parser LT.LowType
lowTypeStruct = do
  keyword "struct"
  LT.Struct <$> argList lowType

lowTypeNumber :: Parser LT.LowType
lowTypeNumber = do
  LT.PrimNum <$> primType

primType :: Parser PT.PrimType
primType = do
  sizeString <- symbol
  case PT.fromText sizeString of
    Just primNum ->
      return primNum
    _ -> do
      failure (Just (asTokens sizeString)) (S.fromList [asLabel "i{n}", asLabel "f{n}"])

rawTermMatch :: Parser RT.RawTerm
rawTermMatch = do
  m <- getCurrentHint
  keyword "match"
  es <- commaList rawTerm
  patternRowList <- betweenBrace $ manyList $ rawTermPatternRow (length es)
  return $ m :< RT.DataElim False es (RP.new patternRowList)

rawTermMatchNoetic :: Parser RT.RawTerm
rawTermMatchNoetic = do
  m <- getCurrentHint
  keyword "match-noetic"
  es <- commaList rawTerm
  patternRowList <- betweenBrace $ manyList $ rawTermPatternRow (length es)
  return $ m :< RT.DataElim True es (RP.new patternRowList)

rawTermPatternRow :: Int -> Parser (RP.RawPatternRow RT.RawTerm)
rawTermPatternRow patternSize = do
  m <- getCurrentHint
  patternList <- commaList rawTermPattern
  unless (length patternList == patternSize) $ do
    lift $
      Throw.raiseError m $
        "the size of the pattern row `"
          <> T.pack (show (length patternList))
          <> "` doesn't match with its input size `"
          <> T.pack (show patternSize)
          <> "`"
          <> "\n"
          <> T.pack (show patternList)
  delimiter "=>"
  body <- rawTerm
  return (V.fromList patternList, body)

rawTermPattern :: Parser (Hint, RP.RawPattern)
rawTermPattern = do
  choice [try rawTermPatternConsStrict, try rawTermPatternCons, rawTermPatternVar]

rawTermPatternConsStrict :: Parser (Hint, RP.RawPattern)
rawTermPatternConsStrict = do
  (m, globalLocator, localLocator) <- parseDefiniteDescription
  patArgs <- argList rawTermPattern
  return (m, RP.Cons (RP.LocatorPair globalLocator localLocator) patArgs)

rawTermPatternCons :: Parser (Hint, RP.RawPattern)
rawTermPatternCons = do
  m <- getCurrentHint
  c <- symbol
  patArgs <- argList rawTermPattern
  return (m, RP.Cons (RP.UnresolvedName $ UN.UnresolvedName c) patArgs)

rawTermPatternVar :: Parser (Hint, RP.RawPattern)
rawTermPatternVar = do
  m <- getCurrentHint
  varText <- symbol
  return (m, RP.Var (Ident.fromText varText))

rawTermNew :: Parser RT.RawTerm
rawTermNew = do
  m <- getCurrentHint
  keyword "new"
  name <- symbol
  rowList <- betweenBrace $ manyList rawTermNewRow
  return $ m :< RT.New name rowList

rawTermNewRow :: Parser (Hint, T.Text, RT.RawTerm)
rawTermNewRow = do
  m <- getCurrentHint
  key <- symbol
  delimiter "<="
  value <- rawTerm
  return (m, key, value)

rawTermLetVar :: Parser (BinderF RT.RawTerm)
rawTermLetVar = do
  m <- getCurrentHint
  choice
    [ try $ do
        x <- symbol
        delimiter ":"
        a <- rawTerm
        return (m, Ident.fromText x, a),
      do
        x <- symbol
        h <- lift $ Gensym.newPreHole m
        return (m, Ident.fromText x, h)
    ]

rawTermIf :: Parser RT.RawTerm
rawTermIf = do
  m <- getCurrentHint
  try $ keyword "if"
  ifCond <- rawTerm
  ifBody <- betweenBrace $ rawTerm
  elseIfList <- many $ do
    keyword "else-if"
    elseIfCond <- rawTerm
    elseIfBody <- betweenBrace $ rawTerm
    return (elseIfCond, elseIfBody)
  keyword "else"
  elseBody <- betweenBrace $ rawTerm
  boolTrue <- lift $ handleDefiniteDescriptionIntoRawConsName m coreBoolTrue
  boolFalse <- lift $ handleDefiniteDescriptionIntoRawConsName m coreBoolFalse
  return $ foldIf m boolTrue boolFalse ifCond ifBody elseIfList elseBody

foldIf ::
  Hint ->
  RP.RawConsName ->
  RP.RawConsName ->
  RT.RawTerm ->
  RT.RawTerm ->
  [(RT.RawTerm, RT.RawTerm)] ->
  RT.RawTerm ->
  RT.RawTerm
foldIf m true false ifCond ifBody elseIfList elseBody =
  case elseIfList of
    [] -> do
      m
        :< RT.DataElim
          False
          [ifCond]
          ( RP.new
              [ (V.fromList [(m, RP.Cons true [])], ifBody),
                (V.fromList [(m, RP.Cons false [])], elseBody)
              ]
          )
    ((elseIfCond, elseIfBody) : rest) -> do
      let cont = foldIf m true false elseIfCond elseIfBody rest elseBody
      m
        :< RT.DataElim
          False
          [ifCond]
          ( RP.new
              [ (V.fromList [(m, RP.Cons true [])], ifBody),
                (V.fromList [(m, RP.Cons false [])], cont)
              ]
          )

rawTermParen :: Parser RT.RawTerm
rawTermParen = do
  m <- getCurrentHint
  es <- argList rawTerm
  case es of
    [e] ->
      return e
    _ ->
      lift $ Throw.raiseError m "found a non-singleton tuple"

bind :: BinderF RT.RawTerm -> RT.RawTerm -> RT.RawTerm -> RT.RawTerm
bind mxt@(m, _, _) e cont =
  m :< RT.Let mxt [] e cont

rawTermNoema :: Parser RT.RawTerm
rawTermNoema = do
  m <- getCurrentHint
  delimiter "&"
  t <- rawTermBasic'
  return $ m :< RT.Noema t

rawTermAdmit :: Parser RT.RawTerm
rawTermAdmit = do
  m <- getCurrentHint
  try $ keyword "admit"
  h <- lift $ Gensym.newPreHole m
  return $
    m
      :< RT.PiElim
        (preVar m "core.os.exit")
        [ h,
          m :< RT.Prim (WP.Value (WPV.Int (RT.i64 m) 1))
        ]

rawTermPiElim :: Parser RT.RawTerm
rawTermPiElim = do
  m <- getCurrentHint
  e <- rawTermSimple
  elems <- some $ choice [rawTermPiElimForwardBracket, rawTermPiElimBackwardBracket]
  foldPiElimBracket m e elems

data PiElimBracket
  = PiElimBracketForward [RT.RawTerm] -- f<imp-arg-1, ..., imp-arg-n>(arg-1, ..., arg-m)
  | PiElimBracketBackward RT.RawTerm -- e[f]

rawTermPiElimForwardBracket :: Parser PiElimBracket
rawTermPiElimForwardBracket = do
  es <- argList rawTerm
  return $ PiElimBracketForward es

rawTermPiElimBackwardBracket :: Parser PiElimBracket
rawTermPiElimBackwardBracket = do
  f <- betweenBracket rawTerm
  return $ PiElimBracketBackward f

foldPiElimBracket :: Hint -> RT.RawTerm -> [PiElimBracket] -> Parser RT.RawTerm
foldPiElimBracket m e elemList =
  case elemList of
    [] ->
      return e
    bracket : rest ->
      case bracket of
        PiElimBracketForward args ->
          foldPiElimBracket m (m :< RT.PiElim e args) rest
        PiElimBracketBackward func ->
          foldPiElimBracket m (m :< RT.PiElim func [e]) rest

--
-- term-related helper functions
--

preBinder :: Parser (BinderF RT.RawTerm)
preBinder =
  choice
    [ try preAscription,
      preAscription'
    ]

preAscription :: Parser (BinderF RT.RawTerm)
preAscription = do
  m <- getCurrentHint
  x <- symbol
  delimiter ":"
  a <- rawTerm
  return (m, Ident.fromText x, a)

typeWithoutIdent :: Parser (BinderF RT.RawTerm)
typeWithoutIdent = do
  m <- getCurrentHint
  x <- lift $ Gensym.newTextualIdentFromText "_"
  t <- rawTerm
  return (m, x, t)

preAscription' :: Parser (BinderF RT.RawTerm)
preAscription' = do
  (m, x) <- preSimpleIdent
  h <- lift $ Gensym.newPreHole m
  return (m, x, h)

preSimpleIdent :: Parser (Hint, Ident)
preSimpleIdent = do
  m <- getCurrentHint
  x <- symbol
  return (m, Ident.fromText x)

rawTermListIntro :: Parser RT.RawTerm
rawTermListIntro = do
  m <- getCurrentHint
  es <- betweenBracket $ commaList rawTerm
  return $ foldListApp m es

foldListApp :: Hint -> [RT.RawTerm] -> RT.RawTerm
foldListApp m es =
  case es of
    [] ->
      m :< RT.PiElim (m :< RT.Var (Ident.fromText "list.nil")) []
    e : rest ->
      m :< RT.PiElim (m :< RT.Var (Ident.fromText "list.cons")) [e, foldListApp m rest]

rawTermIntrospect :: Parser RT.RawTerm
rawTermIntrospect = do
  m <- getCurrentHint
  try $ keyword "introspect"
  key <- symbol
  value <- lift $ getIntrospectiveValue m key
  clauseList <- betweenBrace $ manyList rawTermIntrospectiveClause
  case lookup value clauseList of
    Just clause ->
      return clause
    Nothing -> do
      lift $ Throw.raiseError m $ "a clause for `" <> value <> "` is missing"

rawTermIntrospectiveClause :: Parser (T.Text, RT.RawTerm)
rawTermIntrospectiveClause = do
  c <- symbol
  delimiter "=>"
  body <- rawTerm
  return (c, body)

getIntrospectiveValue :: Hint -> T.Text -> App T.Text
getIntrospectiveValue m key = do
  tp <- Env.getTargetPlatform
  case key of
    "target-platform" -> do
      return $ T.pack (TP.platform tp)
    "target-arch" ->
      return $ T.pack (TP.arch tp)
    "target-os" ->
      return $ T.pack (TP.os tp)
    _ ->
      Throw.raiseError m $ "no such introspective value is defined: " <> key

rawTermVar :: Parser RT.RawTerm
rawTermVar = do
  (m, x) <- var
  return (preVar m x)

rawTermTextIntro :: Parser RT.RawTerm
rawTermTextIntro = do
  m <- getCurrentHint
  s <- string
  let i8s = encode $ T.unpack s
  let i8 = m :< RT.Prim (WP.Type (PT.Int (PNS.IntSize 8)))
  let i8s' = map (\x -> m :< RT.Prim (WP.Value (WPV.Int i8 (toInteger x)))) i8s
  return $ m :< RT.PiElim (m :< RT.Var (Ident.fromText "text-new")) [foldListApp m i8s']

rawTermInteger :: Parser RT.RawTerm
rawTermInteger = do
  m <- getCurrentHint
  intValue <- try integer
  h <- lift $ Gensym.newPreHole m
  return $ m :< RT.Prim (WP.Value (WPV.Int h intValue))

rawTermFloat :: Parser RT.RawTerm
rawTermFloat = do
  m <- getCurrentHint
  floatValue <- try float
  h <- lift $ Gensym.newPreHole m
  return $ m :< RT.Prim (WP.Value (WPV.Float h floatValue))

lam :: Hint -> [BinderF RT.RawTerm] -> RT.RawTerm -> RT.RawTerm
lam m varList e =
  m :< RT.PiIntro (LK.Normal O.Transparent) varList e

preVar :: Hint -> T.Text -> RT.RawTerm
preVar m str =
  m :< RT.Var (Ident.fromText str)

preVar' :: Hint -> Ident -> RT.RawTerm
preVar' m ident =
  m :< RT.Var ident

handleDefiniteDescriptionIntoRawConsName :: Hint -> T.Text -> App RP.RawConsName
handleDefiniteDescriptionIntoRawConsName m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair m text
  return $ RP.LocatorPair gl ll

handleDefiniteDescriptionIntoVarGlobal :: Hint -> T.Text -> App RT.RawTerm
handleDefiniteDescriptionIntoVarGlobal m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair m text
  return $ m :< RT.VarGlobal gl ll
