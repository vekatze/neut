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
import Entity.DataInfo qualified as DI
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
import Entity.PrimType qualified as PT
import Entity.PrimType.FromText qualified as PT
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.TargetPlatform qualified as TP
import Entity.UnresolvedName qualified as UN
import Entity.WeakArrayKind as WAK
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Scene.Parse.Core
import Text.Megaparsec

--
-- parser for RT.RawTerm
--

rawTerm :: Context m => Parser m RT.RawTerm
rawTerm = do
  m <- getCurrentHint
  e1 <- rawTermBasic
  choice
    [ rawTermSeq m e1,
      rawTermExplicitAscription m e1,
      return e1
    ]

rawTermBasic :: Context m => Parser m RT.RawTerm
rawTermBasic = do
  choice
    [ rawTermPiIntro,
      rawTermPiIntroDef,
      rawTermArray,
      rawTermArrayIntro,
      rawTermArrayElim,
      rawTermVector,
      rawTermVectorIntro,
      rawTermIntrospect,
      rawTermMagic,
      rawTermMatchNoetic,
      rawTermMatch,
      rawTermNew,
      rawTermIf,
      rawTermLetCoproduct,
      try rawTermLetOn,
      rawTermLet,
      try rawTermPi,
      try rawTermVectorElim,
      rawTermBasic'
    ]

rawTermBasic' :: Context m => Parser m RT.RawTerm
rawTermBasic' = do
  choice
    [ rawTermNoema,
      rawTermEmbody,
      try rawTermPiElim,
      rawTermSimple
    ]

rawTermSimple :: Context m => Parser m RT.RawTerm
rawTermSimple = do
  choice
    [ rawTermParen,
      rawTermTau,
      rawTermAdmit,
      rawTermHole,
      rawTermInteger,
      rawTermFloat,
      rawTermDefiniteDescription,
      rawTermVar
    ]

rawTermLetOn :: Context m => Parser m RT.RawTerm
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

rawTermLet :: Context m => Parser m RT.RawTerm
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
rawTermLetCoproduct :: Context m => Parser m RT.RawTerm
rawTermLetCoproduct = do
  m <- getCurrentHint
  try $ keyword "let?"
  x <- Ident.fromText <$> symbol
  delimiter "="
  e1 <- rawTerm
  keyword "in"
  e2 <- rawTerm
  err <- lift $ Gensym.newTextualIdentFromText "err"
  globalLocator <- lift $ GL.reflect m "base.coproduct"
  localLocator <- lift $ LL.reflect m "coproduct.left"
  let sumLeftVar = m :< RT.VarGlobal globalLocator localLocator
  return $
    m
      :< RT.DataElim
        False
        [e1]
        ( RP.new
            [ ( V.fromList [(m, RP.Cons (RP.DefiniteDescription DI.constCoproductLeft) [(m, RP.Var err)])],
                m :< RT.PiElim sumLeftVar [preVar' m err]
              ),
              ( V.fromList [(m, RP.Cons (RP.DefiniteDescription DI.constCoproductRight) [(m, RP.Var x)])],
                e2
              )
            ]
        )

rawTermEmbody :: Context m => Parser m RT.RawTerm
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

rawTermSeq :: Context m => Hint -> RT.RawTerm -> Parser m RT.RawTerm
rawTermSeq m e1 = do
  delimiter ";"
  e2 <- rawTerm
  f <- lift $ Gensym.newTextualIdentFromText "unit"
  return $ bind (m, f, m :< RT.Data DI.constTop []) e1 e2

rawTermExplicitAscription :: Context m => Hint -> RT.RawTerm -> Parser m RT.RawTerm
rawTermExplicitAscription m e = do
  delimiter ":"
  t <- rawTermBasic
  f <- lift $ Gensym.newTextualIdentFromText "unit"
  return $ bind (m, f, t) e (m :< RT.Var f)

rawTermTau :: Context m => Parser m RT.RawTerm
rawTermTau = do
  m <- getCurrentHint
  try $ keyword "tau"
  return $ m :< RT.Tau

rawTermHole :: Context m => Parser m RT.RawTerm
rawTermHole = do
  m <- getCurrentHint
  delimiter "?"
  lift $ Gensym.newPreHole m

rawTermPi :: Context m => Parser m RT.RawTerm
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

rawTermPiIntro :: Context m => Parser m RT.RawTerm
rawTermPiIntro = do
  m <- getCurrentHint
  try $ keyword "lambda"
  varList <- argList preBinder
  e <- choice [rawTermDotBind, doBlock rawTerm]
  return $ lam m varList e

rawTermDotBind :: Context m => Parser m RT.RawTerm
rawTermDotBind = do
  delimiter "."
  rawTerm

parseDefInfo :: Context m => Parser m RT.DefInfo
parseDefInfo = do
  functionVar <- var
  domInfoList <- argList preBinder
  delimiter ":"
  codType <- rawTerm
  e <- equalBlock rawTerm
  return (functionVar, domInfoList, codType, e)

parseTopDefInfo :: Context m => Parser m RT.TopDefInfo
parseTopDefInfo = do
  m <- getCurrentHint
  funcBaseName <- baseName
  impDomInfoList <- impArgList preBinder
  domInfoList <- argList preBinder
  delimiter ":"
  codType <- rawTerm
  e <- equalBlock rawTerm
  return ((m, funcBaseName), impDomInfoList, domInfoList, codType, e)

-- define name(x1: A1, ..., xn: An)[: A] as e end
rawTermPiIntroDef :: Context m => Parser m RT.RawTerm
rawTermPiIntroDef = do
  m <- getCurrentHint
  try $ keyword "define"
  ((mFun, functionName), domBinderList, codType, e) <- parseDefInfo
  let piType = mFun :< RT.Pi domBinderList codType
  return $ m :< RT.PiIntro (LK.Fix (mFun, Ident.fromText functionName, piType)) domBinderList e

parseDefiniteDescription :: Context m => Parser m (Hint, GL.GlobalLocator, LL.LocalLocator)
parseDefiniteDescription = do
  m <- getCurrentHint
  globalLocator <- symbol
  globalLocator' <- lift $ GL.reflect m globalLocator
  delimiter definiteSep
  localLocator <- parseLocalLocator
  return (m, globalLocator', localLocator)

rawTermDefiniteDescription :: Context m => Parser m RT.RawTerm
rawTermDefiniteDescription = do
  (m, globalLocator, localLocator) <- try parseDefiniteDescription
  return $ m :< RT.VarGlobal globalLocator localLocator

parseLocalLocator :: Context m => Parser m LL.LocalLocator
parseLocalLocator = do
  m <- getCurrentHint
  rawTxt <- symbol
  lift $ LL.reflect m rawTxt

rawTermMagic :: Context m => Parser m RT.RawTerm
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

rawTermMagicBase :: Context m => T.Text -> Parser m RT.RawTerm -> Parser m RT.RawTerm
rawTermMagicBase k parser = do
  keyword k
  betweenParen parser

rawTermMagicCast :: Context m => Hint -> Parser m RT.RawTerm
rawTermMagicCast m = do
  rawTermMagicBase "cast" $ do
    castFrom <- rawTerm
    castTo <- delimiter "," >> rawTerm
    value <- delimiter "," >> rawTerm
    return $ m :< RT.Magic (M.Cast castFrom castTo value)

rawTermMagicStore :: Context m => Hint -> Parser m RT.RawTerm
rawTermMagicStore m = do
  rawTermMagicBase "store" $ do
    lt <- lowType
    pointer <- delimiter "," >> rawTerm
    value <- delimiter "," >> rawTerm
    return $ m :< RT.Magic (M.Store lt pointer value)

rawTermMagicLoad :: Context m => Hint -> Parser m RT.RawTerm
rawTermMagicLoad m = do
  rawTermMagicBase "load" $ do
    lt <- lowType
    pointer <- delimiter "," >> rawTerm
    return $ m :< RT.Magic (M.Load lt pointer)

rawTermMagicSyscall :: Context m => Hint -> Parser m RT.RawTerm
rawTermMagicSyscall m = do
  rawTermMagicBase "syscall" $ do
    syscallNum <- integer
    es <- many (delimiter "," >> rawTerm)
    return $ m :< RT.Magic (M.Syscall syscallNum es)

rawTermMagicExternal :: Context m => Hint -> Parser m RT.RawTerm
rawTermMagicExternal m = do
  rawTermMagicBase "external" $ do
    extFunName <- symbol
    es <- many (delimiter "," >> rawTerm)
    return $ m :< RT.Magic (M.External (EN.ExternalName extFunName) es)

-- -- t ::= i{n} | f{n} | pointer t | array INT t | struct t ... t
lowType :: Context m => Parser m LT.LowType
lowType = do
  choice
    [ lowTypePointer,
      lowTypeArray,
      lowTypeStruct,
      lowTypeNumber
    ]

lowTypePointer :: Context m => Parser m LT.LowType
lowTypePointer = do
  keyword "pointer"
  LT.Pointer <$> betweenParen lowType

lowTypeArray :: Context m => Parser m LT.LowType
lowTypeArray = do
  keyword "array"
  betweenParen $ do
    intValue <- integer
    delimiter ","
    LT.Array (fromInteger intValue) <$> lowType

lowTypeStruct :: Context m => Parser m LT.LowType
lowTypeStruct = do
  keyword "struct"
  LT.Struct <$> argList lowType

lowTypeNumber :: Context m => Parser m LT.LowType
lowTypeNumber = do
  LT.PrimNum <$> primType

primType :: Context m => Parser m PT.PrimType
primType = do
  sizeString <- symbol
  case PT.fromText sizeString of
    Just primNum ->
      return primNum
    _ -> do
      failure (Just (asTokens sizeString)) (S.fromList [asLabel "i{n}", asLabel "f{n}"])

rawTermMatch :: (Context m, Throw.Context m) => Parser m RT.RawTerm
rawTermMatch = do
  m <- getCurrentHint
  keyword "match"
  es <- sepByTill rawTerm (delimiter ",") (keyword "with")
  patternRowList <- manyList $ rawTermPatternRow (length es)
  keyword "end"
  return $ m :< RT.DataElim False es (RP.new patternRowList)

rawTermMatchNoetic :: (Context m, Throw.Context m) => Parser m RT.RawTerm
rawTermMatchNoetic = do
  m <- getCurrentHint
  keyword "match-noetic"
  es <- sepByTill rawTerm (delimiter ",") (keyword "with")
  patternRowList <- manyList $ rawTermPatternRow (length es)
  keyword "end"
  return $ m :< RT.DataElim True es (RP.new patternRowList)

rawTermPatternRow :: Context m => Int -> Parser m (RP.RawPatternRow RT.RawTerm)
rawTermPatternRow patternSize = do
  m <- getCurrentHint
  patternList <- sepByTill rawTermPattern (delimiter ",") (delimiter "->")
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
  body <- rawTerm
  return (V.fromList patternList, body)

rawTermPattern :: Context m => Parser m (Hint, RP.RawPattern)
rawTermPattern = do
  choice [try rawTermPatternConsStrict, try rawTermPatternCons, rawTermPatternVar]

rawTermPatternConsStrict :: Context m => Parser m (Hint, RP.RawPattern)
rawTermPatternConsStrict = do
  (m, globalLocator, localLocator) <- parseDefiniteDescription
  patArgs <- argList rawTermPattern
  return (m, RP.Cons (RP.LocatorPair globalLocator localLocator) patArgs)

rawTermPatternCons :: Context m => Parser m (Hint, RP.RawPattern)
rawTermPatternCons = do
  m <- getCurrentHint
  c <- symbol
  patArgs <- argList rawTermPattern
  return (m, RP.Cons (RP.UnresolvedName $ UN.UnresolvedName c) patArgs)

rawTermPatternVar :: Context m => Parser m (Hint, RP.RawPattern)
rawTermPatternVar = do
  m <- getCurrentHint
  varText <- symbol
  return (m, RP.Var (Ident.fromText varText))

rawTermNew :: (Context m, Throw.Context m) => Parser m RT.RawTerm
rawTermNew = do
  m <- getCurrentHint
  keyword "new"
  name <- symbol
  keyword "with"
  rowList <- manyList rawTermNewRow
  keyword "end"
  return $ m :< RT.New name rowList

rawTermNewRow :: Context m => Parser m (Hint, T.Text, RT.RawTerm)
rawTermNewRow = do
  m <- getCurrentHint
  key <- symbol
  delimiter "<-"
  value <- rawTerm
  return (m, key, value)

rawTermLetVar :: Context m => Parser m (BinderF RT.RawTerm)
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

rawTermIf :: Context m => Parser m RT.RawTerm
rawTermIf = do
  m <- getCurrentHint
  try $ keyword "if"
  ifCond <- rawTerm
  keyword "then"
  ifBody <- rawTerm
  elseIfList <- many $ do
    keyword "else-if"
    elseIfCond <- rawTerm
    keyword "then"
    elseIfBody <- rawTerm
    return (elseIfCond, elseIfBody)
  keyword "else"
  elseBody <- rawTerm
  keyword "end"
  return $ foldIf m ifCond ifBody elseIfList elseBody

foldIf :: Hint -> RT.RawTerm -> RT.RawTerm -> [(RT.RawTerm, RT.RawTerm)] -> RT.RawTerm -> RT.RawTerm
foldIf m ifCond ifBody elseIfList elseBody =
  case elseIfList of
    [] -> do
      m
        :< RT.DataElim
          False
          [ifCond]
          ( RP.new
              [ (V.fromList [(m, RP.Cons (RP.DefiniteDescription DI.constBoolTrue) [])], ifBody),
                (V.fromList [(m, RP.Cons (RP.DefiniteDescription DI.constBoolFalse) [])], elseBody)
              ]
          )
    ((elseIfCond, elseIfBody) : rest) -> do
      let cont = foldIf m elseIfCond elseIfBody rest elseBody
      m
        :< RT.DataElim
          False
          [ifCond]
          ( RP.new
              [ (V.fromList [(m, RP.Cons (RP.DefiniteDescription DI.constBoolTrue) [])], ifBody),
                (V.fromList [(m, RP.Cons (RP.DefiniteDescription DI.constBoolFalse) [])], cont)
              ]
          )

rawTermParen :: Context m => Parser m RT.RawTerm
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

rawTermNoema :: Context m => Parser m RT.RawTerm
rawTermNoema = do
  m <- getCurrentHint
  delimiter "&"
  t <- rawTermBasic'
  return $ m :< RT.Noema t

rawTermAdmit :: Context m => Parser m RT.RawTerm
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

rawTermPiElim :: Context m => Parser m RT.RawTerm
rawTermPiElim = do
  m <- getCurrentHint
  e <- rawTermSimple
  elems <- some $ choice [rawTermPiElimForwardBracket, rawTermPiElimBackwardBracket]
  foldPiElimBracket m e elems

data PiElimBracket
  = PiElimBracketForward [RT.RawTerm] [RT.RawTerm] -- f<imp-arg-1, ..., imp-arg-n>(arg-1, ..., arg-m)
  | PiElimBracketBackward RT.RawTerm -- e[f]

rawTermPiElimForwardBracket :: Context m => Parser m PiElimBracket
rawTermPiElimForwardBracket = do
  impBrackets <- impArgList rawTerm
  es <- argList rawTerm
  return $ PiElimBracketForward impBrackets es

rawTermPiElimBackwardBracket :: Context m => Parser m PiElimBracket
rawTermPiElimBackwardBracket = do
  f <- betweenBracket rawTerm
  return $ PiElimBracketBackward f

foldPiElimBracket :: Context m => Hint -> RT.RawTerm -> [PiElimBracket] -> Parser m RT.RawTerm
foldPiElimBracket m e elemList =
  case elemList of
    [] ->
      return e
    bracket : rest ->
      case bracket of
        PiElimBracketForward impArgs args ->
          if null impArgs
            then foldPiElimBracket m (m :< RT.PiElim e args) rest
            else do
              f <- lift $ Gensym.newTextualIdentFromText "func"
              h <- lift $ Gensym.newPreHole m
              let e' = m :< RT.Let (m, f, h) [] e (m :< RT.PiElim (m :< RT.Var f) (impArgs ++ args))
              foldPiElimBracket m e' rest
        PiElimBracketBackward func ->
          foldPiElimBracket m (m :< RT.PiElim func [e]) rest

--
-- term-related helper functions
--

preBinder :: Context m => Parser m (BinderF RT.RawTerm)
preBinder =
  choice
    [ try preAscription,
      preAscription'
    ]

preAscription :: Context m => Parser m (BinderF RT.RawTerm)
preAscription = do
  m <- getCurrentHint
  x <- symbol
  delimiter ":"
  a <- rawTerm
  return (m, Ident.fromText x, a)

typeWithoutIdent :: Context m => Parser m (BinderF RT.RawTerm)
typeWithoutIdent = do
  m <- getCurrentHint
  x <- lift $ Gensym.newTextualIdentFromText "_"
  t <- rawTerm
  return (m, x, t)

preAscription' :: Context m => Parser m (BinderF RT.RawTerm)
preAscription' = do
  (m, x) <- preSimpleIdent
  h <- lift $ Gensym.newPreHole m
  return (m, x, h)

preSimpleIdent :: Context m => Parser m (Hint, Ident)
preSimpleIdent = do
  m <- getCurrentHint
  x <- symbol
  return (m, Ident.fromText x)

rawTermVector :: Context m => Parser m RT.RawTerm
rawTermVector = do
  m <- getCurrentHint
  keyword "vector"
  t <- betweenParen rawTerm
  return $ m :< RT.Array (WAK.General t)

rawTermArray :: Context m => Parser m RT.RawTerm
rawTermArray = do
  m <- getCurrentHint
  keyword "array"
  t <- betweenParen rawTerm
  return $ m :< RT.Array (WAK.PrimType t)

rawTermVectorIntro :: Context m => Parser m RT.RawTerm
rawTermVectorIntro = do
  m <- getCurrentHint
  es <- betweenBracket $ commaList rawTerm
  t <- lift $ Gensym.newPreHole m
  return $ m :< RT.ArrayIntro (WAK.General t) es

rawTermArrayIntro :: Context m => Parser m RT.RawTerm
rawTermArrayIntro = do
  m <- getCurrentHint
  keyword "new-array"
  es <- betweenBracket $ commaList rawTerm
  t <- lift $ Gensym.newPreHole m
  return $ m :< RT.ArrayIntro (WAK.PrimType t) es

rawTermVectorElim :: Context m => Parser m RT.RawTerm
rawTermVectorElim = do
  m <- getCurrentHint
  array <- rawTermBasic'
  delimiter "@"
  index <- rawTermBasic'
  t <- lift $ Gensym.newPreHole m
  return $ m :< RT.ArrayElim (WAK.General t) array index

rawTermArrayElim :: Context m => Parser m RT.RawTerm
rawTermArrayElim = do
  m <- getCurrentHint
  keyword "access-array"
  arrayAndIndex <- betweenBracket $ commaList rawTerm
  case arrayAndIndex of
    [array, index] -> do
      t <- lift $ Gensym.newPreHole m
      return $ m :< RT.ArrayElim (WAK.PrimType t) array index
    _ ->
      lift $
        Throw.raiseError m $
          "array-access requires exactly 2 arguments, but found "
            <> T.pack (show (length arrayAndIndex))
            <> "."

-- array <- rawTermBasic'
-- index <- rawTermBasic'
-- return $ m :< RT.ArrayElim True array index

rawTermIntrospect :: Context m => Parser m RT.RawTerm
rawTermIntrospect = do
  m <- getCurrentHint
  try $ keyword "introspect"
  key <- symbol
  value <- lift $ getIntrospectiveValue m key
  keyword "with"
  clauseList <- many rawTermIntrospectiveClause
  keyword "end"
  case lookup value clauseList of
    Just clause ->
      return clause
    Nothing -> do
      lift $ Throw.raiseError m $ "a clause for `" <> value <> "` is missing"

rawTermIntrospectiveClause :: Context m => Parser m (T.Text, RT.RawTerm)
rawTermIntrospectiveClause = do
  delimiter "-"
  c <- symbol
  delimiter "->"
  body <- rawTerm
  return (c, body)

getIntrospectiveValue :: Context m => Hint -> T.Text -> m T.Text
getIntrospectiveValue m key = do
  tp <- getTargetPlatform
  case key of
    "target-platform" -> do
      return $ T.pack (TP.platform tp)
    "target-arch" ->
      return $ T.pack (TP.arch tp)
    "target-os" ->
      return $ T.pack (TP.os tp)
    _ ->
      Throw.raiseError m $ "no such introspective value is defined: " <> key

rawTermVar :: Context m => Parser m RT.RawTerm
rawTermVar = do
  (m, x) <- var
  return (preVar m x)

-- rawTermText :: Context m => Parser m RT.RawTerm
-- rawTermText = do
--   m <- getCurrentHint
--   try $ keyword "text"
--   return $ m :< RT.Text

-- rawTermTextIntro :: Context m => Parser m RT.RawTerm
-- rawTermTextIntro = do
--   m <- getCurrentHint
--   s <- string
--   return $ m :< RT.TextIntro s

rawTermInteger :: Context m => Parser m RT.RawTerm
rawTermInteger = do
  m <- getCurrentHint
  intValue <- try integer
  h <- lift $ Gensym.newPreHole m
  return $ m :< RT.Prim (WP.Value (WPV.Int h intValue))

rawTermFloat :: Context m => Parser m RT.RawTerm
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
