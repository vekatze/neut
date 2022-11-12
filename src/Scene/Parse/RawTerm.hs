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
import Control.Monad.Trans
import Data.List
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Binder
import Entity.Const
import qualified Entity.Discriminant as D
import qualified Entity.EnumCase as EC
import Entity.EnumInfo
import qualified Entity.ExternalName as EN
import qualified Entity.GlobalLocator as GL
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reflect as Ident
import qualified Entity.LamKind as LK
import qualified Entity.LocalLocator as LL
import qualified Entity.LowType as LT
import qualified Entity.Magic as M
import Entity.Pattern
import qualified Entity.RawTerm as RT
import qualified Entity.PrimNumType.FromText as PNT
import qualified Entity.TargetPlatform as TP
import qualified Entity.UnresolvedName as UN
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
      rawTermEnumElim,
      rawTermIntrospect,
      rawTermQuestion,
      rawTermMagic,
      rawTermMatch,
      rawTermIf,
      try rawTermLetSigmaElim,
      rawTermLet,
      rawTermLetCoproduct,
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

rawTermLet :: Context m => Parser m RT.RawTerm
rawTermLet = do
  m <- getCurrentHint
  try $ keyword "let"
  x <- rawTermLetVar
  delimiter "="
  e1 <- rawTerm
  keyword "in"
  e2 <- rawTerm
  return $ m :< RT.Let x e1 e2

-- let? x : A = e1 in e2
-- let? x     = e1 in e2
rawTermLetCoproduct :: Context m => Parser m RT.RawTerm
rawTermLetCoproduct = do
  m <- getCurrentHint
  try $ keyword "let?"
  x <- rawTermLetVar
  delimiter "="
  e1 <- rawTerm
  keyword "in"
  e2 <- rawTerm
  err <- lift $ Gensym.newTextualIdentFromText "err"
  typeOfLeft <- lift $ Gensym.newPreAster m
  typeOfRight <- lift $ Gensym.newPreAster m
  let sumLeft = Left $ UN.UnresolvedName "base.sum::left"
  let sumRight = Left $ UN.UnresolvedName "base.sum::right"
  let sumLeftVar = Ident.fromText "sum.left"
  return $
    m
      :< RT.Match
        (e1, doNotCare m)
        [ ( (m, sumLeft, [(m, err, typeOfLeft)]),
            m :< RT.PiElim (preVar' m sumLeftVar) [typeOfLeft, typeOfRight, preVar' m err]
          ),
          ( (m, sumRight, [x]),
            e2
          )
        ]

rawTermVoid :: Context m => Hint -> RT.RawTerm -> Parser m RT.RawTerm
rawTermVoid m e1 = do
  delimiter ";"
  e2 <- rawTerm
  f <- lift $ Gensym.newTextualIdentFromText "unit"
  return $ bind (m, f, m :< RT.Enum constTop) e1 e2

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
  e <- asBlock rawTerm
  return (functionVar, domInfoList, codType, e)

parseTopDefInfo :: Context m => Parser m RT.TopDefInfo
parseTopDefInfo = do
  m <- getCurrentHint
  funcBaseName <- baseName
  impDomInfoList <- impArgList preBinder
  domInfoList <- argList preBinder
  delimiter ":"
  codType <- rawTerm
  e <- asBlock rawTerm
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

rawTermEnumElim :: Context m => Parser m RT.RawTerm
rawTermEnumElim = do
  m <- getCurrentHint
  try $ keyword "switch"
  e <- rawTerm
  keyword "with"
  clauseList <- many rawTermEnumClause
  keyword "end"
  h <- lift $ Gensym.newPreAster m
  return $ m :< RT.EnumElim (e, h) clauseList

rawTermEnumClause :: Context m => Parser m (EC.PreEnumCase, RT.RawTerm)
rawTermEnumClause = do
  m <- getCurrentHint
  delimiter "-"
  c <- symbol
  delimiter "->"
  body <- rawTerm
  case c of
    "default" ->
      return (m :< EC.Default, body)
    _ ->
      return (m :< EC.Label (dummyLabel c), body)

dummyLabel :: T.Text -> EC.PreEnumLabel
dummyLabel c =
  EC.PreEnumLabel
    (UN.UnresolvedName "base.top::unit")
    D.zero
    (UN.UnresolvedName c)

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
  case PNT.fromText sizeString of
    Just primNum ->
      return $ LT.PrimNum primNum
    _ -> do
      failure (Just (asTokens sizeString)) (S.fromList [asLabel "i{n}", asLabel "f{n}"])

rawTermMatch :: Context m => Parser m RT.RawTerm
rawTermMatch = do
  m <- getCurrentHint
  try $ keyword "match"
  e <- rawTerm
  clauseList <- withBlock $ manyList rawTermMatchClause
  return $ m :< RT.Match (e, doNotCare m) clauseList

rawTermMatchClause :: Context m => Parser m (PrePatternF RT.RawTerm, RT.RawTerm)
rawTermMatchClause = do
  pat <- rawTermPattern
  delimiter "->"
  body <- rawTerm
  return (pat, body)

rawTermPattern :: Context m => Parser m (PrePatternF RT.RawTerm)
rawTermPattern = do
  m <- getCurrentHint
  c <- symbol
  patArgs <- argList preBinder
  return (m, Left $ UN.UnresolvedName c, patArgs)

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
  foldIf m ifCond ifBody elseIfList elseBody

foldIf :: Context m => Hint -> RT.RawTerm -> RT.RawTerm -> [(RT.RawTerm, RT.RawTerm)] -> RT.RawTerm -> Parser m RT.RawTerm
foldIf m ifCond ifBody elseIfList elseBody =
  case elseIfList of
    [] -> do
      h <- lift $ Gensym.newPreAster m
      return $
        m
          :< RT.EnumElim
            (ifCond, h)
            [ (m :< EC.Label (EC.weakenEnumLabel EC.enumLabelBoolTrue), ifBody),
              (m :< EC.Label (EC.weakenEnumLabel EC.enumLabelBoolFalse), elseBody)
            ]
    ((elseIfCond, elseIfBody) : rest) -> do
      cont <- foldIf m elseIfCond elseIfBody rest elseBody
      h <- lift $ Gensym.newPreAster m
      return $
        m
          :< RT.EnumElim
            (ifCond, h)
            [ (m :< EC.Label (EC.weakenEnumLabel EC.enumLabelBoolTrue), ifBody),
              (m :< EC.Label (EC.weakenEnumLabel EC.enumLabelBoolFalse), cont)
            ]

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
  m :< RT.Let mxt e cont

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
          m :< RT.Int (RT.i64 m) 1
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
                m :< RT.Int (RT.i64 m) 1
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
  return $ m :< RT.Int h intValue

rawTermFloat :: Context m => Parser m RT.RawTerm
rawTermFloat = do
  m <- getCurrentHint
  floatValue <- try float
  h <- lift $ Gensym.newPreAster m
  return $ m :< RT.Float h floatValue

doNotCare :: Hint -> RT.RawTerm
doNotCare m =
  m :< RT.Tau

lam :: Hint -> [BinderF RT.RawTerm] -> RT.RawTerm -> RT.RawTerm
lam m varList e =
  m :< RT.PiIntro LK.Normal varList e

preVar :: Hint -> T.Text -> RT.RawTerm
preVar m str =
  m :< RT.Var (Ident.fromText str)

preVar' :: Hint -> Ident -> RT.RawTerm
preVar' m ident =
  m :< RT.Var ident

