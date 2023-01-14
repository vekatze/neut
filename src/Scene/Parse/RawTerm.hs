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

import qualified Context.Gensym as Gensym
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Trans
import Data.Bifunctor
import Data.List
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Entity.Binder
import Entity.Const
import qualified Entity.DataInfo as DI
import qualified Entity.ExternalName as EN
import qualified Entity.GlobalLocator as GL
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reflect as Ident
import qualified Entity.LamKind as LK
import qualified Entity.LocalLocator as LL
import qualified Entity.LowType as LT
import qualified Entity.Magic as M
import qualified Entity.Opacity as O
import qualified Entity.PrimType.FromText as PT
import qualified Entity.RawPattern as RP
import qualified Entity.RawTerm as RT
import qualified Entity.TargetPlatform as TP
import qualified Entity.UnresolvedName as UN
import qualified Entity.WeakPrim as WP
import qualified Entity.WeakPrimValue as WPV
import Scene.Parse.Core
import Text.Megaparsec

--
-- parser for RT.RawTerm
--

rawTerm :: Context m => Parser m RT.RawTerm
rawTerm = do
  m <- getCurrentHint
  e1 <- rawTermEasy
  choice
    [ rawTermVoid m e1,
      rawTermExplicitAscription m e1,
      return e1
    ]

-- fixme: easy??
rawTermEasy :: Context m => Parser m RT.RawTerm
rawTermEasy = do
  choice
    [ rawTermPiIntro,
      rawTermPiIntroDef,
      rawTermSigma,
      rawTermSigmaIntro,
      rawTermNoema,
      rawTermIntrospect,
      rawTermQuestion,
      rawTermMagic,
      rawTermMatchNoetic,
      rawTermMatch,
      rawTermIf,
      try rawTermLetSigmaElim,
      rawTermLetCoproduct,
      try rawTermLetOn,
      rawTermLet,
      rawTermEmbody,
      try rawTermPi,
      try rawTermPiElim,
      try rawTermPiElimInv,
      rawTermSimple
    ]

rawTermSimple :: Context m => Parser m RT.RawTerm
rawTermSimple = do
  choice
    [ rawTermParen,
      rawTermTau,
      rawTermAdmitQuestion,
      rawTermAdmit,
      rawTermAster,
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
  e <- rawTermSimple
  t <- lift $ Gensym.newPreAster m
  raw <- lift $ Gensym.newTextualIdentFromText "raw"
  copied <- lift $ Gensym.newTextualIdentFromText "copied"
  original <- lift $ Gensym.newTextualIdentFromText "original"
  return $
    bind (m, raw, t) (m :< RT.Magic (M.Cast (m :< RT.Noema t) t e)) $
      bind (m, original, m :< RT.Noema t) (m :< RT.Magic (M.Cast t (m :< RT.Noema t) (m :< RT.Var raw))) $
        bind (m, copied, t) (m :< RT.Var raw) $
          m :< RT.Var copied

rawTermVoid :: Context m => Hint -> RT.RawTerm -> Parser m RT.RawTerm
rawTermVoid m e1 = do
  delimiter ";"
  e2 <- rawTerm
  f <- lift $ Gensym.newTextualIdentFromText "unit"
  return $ bind (m, f, m :< RT.Data DI.constTop []) e1 e2

rawTermExplicitAscription :: Context m => Hint -> RT.RawTerm -> Parser m RT.RawTerm
rawTermExplicitAscription m e = do
  delimiter ":"
  t <- rawTermEasy
  f <- lift $ Gensym.newTextualIdentFromText "unit"
  return $ bind (m, f, t) e (m :< RT.Var f)

rawTermTau :: Context m => Parser m RT.RawTerm
rawTermTau = do
  m <- getCurrentHint
  try $ keyword "tau"
  return $ m :< RT.Tau

rawTermAster :: Context m => Parser m RT.RawTerm
rawTermAster = do
  m <- getCurrentHint
  delimiter "?"
  lift $ Gensym.newPreAster m

rawTermPi :: Context m => Parser m RT.RawTerm
rawTermPi = do
  m <- getCurrentHint
  domList <- argList $ choice [try preAscription, typeWithoutIdent]
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

rawTermSigma :: Context m => Parser m RT.RawTerm
rawTermSigma = do
  m <- getCurrentHint
  try $ keyword "tuple"
  ts <- argList $ choice [try preAscription, typeWithoutIdent]
  return $ m :< RT.Sigma ts

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

-- question e
rawTermQuestion :: Context m => Parser m RT.RawTerm
rawTermQuestion = do
  m <- getCurrentHint
  try $ keyword "question"
  e <- rawTerm
  h <- lift $ Gensym.newPreAster m
  return $ m :< RT.Question e h

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
      lowTypeSimple
    ]

lowTypeSimple :: Context m => Parser m LT.LowType
lowTypeSimple =
  choice
    [ betweenParen lowType,
      lowTypeNumber
    ]

lowTypePointer :: Context m => Parser m LT.LowType
lowTypePointer = do
  keyword "pointer"
  lowTypeSimple

lowTypeArray :: Context m => Parser m LT.LowType
lowTypeArray = do
  keyword "array"
  intValue <- integer
  LT.Array (fromInteger intValue) <$> lowTypeSimple

lowTypeStruct :: Context m => Parser m LT.LowType
lowTypeStruct = do
  keyword "struct"
  LT.Struct <$> many lowTypeSimple

lowTypeNumber :: Context m => Parser m LT.LowType
lowTypeNumber = do
  sizeString <- symbol
  case PT.fromText sizeString of
    Just primNum ->
      return $ LT.PrimNum primNum
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

-- let (x1 : A1, ..., xn : An) = e1 in e2
rawTermLetSigmaElim :: Context m => Parser m RT.RawTerm
rawTermLetSigmaElim = do
  m <- getCurrentHint
  try $ keyword "let"
  xts <- argList preBinder
  delimiter "="
  e1 <- rawTerm
  keyword "in"
  e2 <- rawTerm
  return $ m :< RT.SigmaElim xts e1 e2

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
        h <- lift $ Gensym.newPreAster m
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
    [] ->
      return $ m :< RT.SigmaIntro []
    [e] ->
      return e
    _ ->
      return $ m :< RT.SigmaIntro es

-- -- (e1, ..., en) (n >= 2)
rawTermSigmaIntro :: Context m => Parser m RT.RawTerm
rawTermSigmaIntro = do
  m <- getCurrentHint
  try $ keyword "tuple-new"
  es <- argList rawTerm
  return $ m :< RT.SigmaIntro es

bind :: BinderF RT.RawTerm -> RT.RawTerm -> RT.RawTerm -> RT.RawTerm
bind mxt@(m, _, _) e cont =
  m :< RT.Let mxt [] e cont

rawTermNoema :: Context m => Parser m RT.RawTerm
rawTermNoema = do
  m <- getCurrentHint
  delimiter "&"
  t <- rawTermEasy
  return $ m :< RT.Noema t

rawTermAdmit :: Context m => Parser m RT.RawTerm
rawTermAdmit = do
  m <- getCurrentHint
  try $ keyword "admit"
  h <- lift $ Gensym.newPreAster m
  return $
    m
      :< RT.PiElim
        (preVar m "core.os.exit")
        [ h,
          m :< RT.Prim (WP.Value (WPV.Int (RT.i64 m) 1))
        ]

rawTermAdmitQuestion :: Context m => Parser m RT.RawTerm
rawTermAdmitQuestion = do
  m <- getCurrentHint
  try $ keyword "?admit"
  h <- lift $ Gensym.newPreAster m
  return $
    m
      :< RT.Question
        ( m
            :< RT.PiElim
              (preVar m "os.exit")
              [ h,
                m :< RT.Prim (WP.Value (WPV.Int (RT.i64 m) 1))
              ]
        )
        h

rawTermPiElim :: Context m => Parser m RT.RawTerm
rawTermPiElim = do
  m <- getCurrentHint
  e <- rawTermSimple
  impArgs <- impArgList rawTerm
  es <- argList rawTerm
  ess <- many $ argList rawTerm
  if null impArgs
    then return $ foldl' (\base args -> m :< RT.PiElim base args) e $ es : ess
    else do
      f <- lift $ Gensym.newTextualIdentFromText "func"
      h <- lift $ Gensym.newPreAster m
      return $
        m
          :< RT.Let
            (m, f, h)
            []
            e
            ( foldl'
                (\base args -> m :< RT.PiElim base args)
                (m :< RT.Var f)
                ((impArgs ++ es) : ess)
            )

rawTermPiElimInv :: Context m => Parser m RT.RawTerm
rawTermPiElimInv = do
  m <- getCurrentHint
  e <- rawTermSimple
  f <- betweenBracket rawTerm
  fs <- many $ betweenBracket rawTerm
  return $ foldl' (\base func -> m :< RT.PiElim func [base]) e $ f : fs

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
  h <- lift $ Gensym.newPreAster m
  return (m, x, h)

preSimpleIdent :: Context m => Parser m (Hint, Ident)
preSimpleIdent = do
  m <- getCurrentHint
  x <- symbol
  return (m, Ident.fromText x)

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
  h <- lift $ Gensym.newPreAster m
  return $ m :< RT.Prim (WP.Value (WPV.Int h intValue))

rawTermFloat :: Context m => Parser m RT.RawTerm
rawTermFloat = do
  m <- getCurrentHint
  floatValue <- try float
  h <- lift $ Gensym.newPreAster m
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

-- castFromNoema :: Context m => RT.RawTerm -> m RT.RawTerm
-- castFromNoema tree@(m :< _) = do
--   baseType <- Gensym.newPreAster m
--   return $ m :< RT.Magic (M.Cast (wrapWithNoema baseType) baseType tree)

-- castToNoema :: RT.RawTerm -> RT.RawTerm -> RT.RawTerm
-- castToNoema baseType tree@(m :< _) = do
--   m :< RT.Magic (M.Cast baseType (wrapWithNoema baseType) tree)

-- wrapWithNoema :: RT.RawTerm -> RT.RawTerm
-- wrapWithNoema baseType@(m :< _) = do
--   m :< RT.Noema baseType
