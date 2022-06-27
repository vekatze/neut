module Scene.Parse.WeakTerm
  ( weakTerm,
    weakTermSimple,
    weakBinder,
    weakAscription,
    parseTopDefInfo,
    parseDefiniteDescription,
    weakVar,
    newTextualIdentFromText,
  )
where

import Control.Comonad.Cofree
import Control.Monad.IO.Class
import Data.IORef
import Data.List
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Binder
import Entity.EnumCase
import Entity.Global
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reflect as Ident
import Entity.LamKind
import Entity.Log
import Entity.LowType
import Entity.Magic
import Entity.Pattern
import Entity.PrimNum.FromText
import Entity.WeakTerm
import Scene.Parse.Core
import Text.Megaparsec

--
-- parser for WeakTerm
--

weakTerm :: Parser WeakTerm
weakTerm = do
  m <- currentHint
  e1 <- weakTermEasy
  choice
    [ weakTermVoid m e1,
      weakTermExplicitAscription m e1,
      return e1
    ]

weakTermEasy :: Parser WeakTerm
weakTermEasy = do
  choice
    [ weakTermPiIntro,
      weakTermPiIntroDef,
      weakTermSigma,
      weakTermSigmaIntro,
      weakTermEnumElim,
      weakTermIntrospect,
      weakTermQuestion,
      weakTermMagic,
      weakTermMatch,
      weakTermMatchNoetic,
      weakTermIf,
      weakTermIdealize,
      weakTermArray,
      weakTermArrayIntro,
      weakTermArrayAccess,
      weakTermText,
      weakTermCell,
      weakTermCellIntro,
      weakTermCellRead,
      weakTermCellWrite,
      weakTermNoema,
      try weakTermLetSigmaElim,
      weakTermLet,
      weakTermLetCoproduct,
      try weakTermPi,
      try weakTermPiElim,
      try weakTermPiElimInv,
      weakTermSimple
    ]

weakTermSimple :: Parser WeakTerm
weakTermSimple = do
  choice
    [ weakTermParen,
      weakTermTau,
      weakTermTextIntro,
      weakTermAdmitQuestion,
      weakTermAdmit,
      weakTermAster,
      weakTermInteger,
      weakTermFloat,
      weakTermDefiniteDescription,
      weakTermVar
    ]

weakTermLet :: Parser WeakTerm
weakTermLet = do
  m <- currentHint
  try $ keyword "let"
  x <- weakTermLetVar
  delimiter "="
  e1 <- weakTerm
  keyword "in"
  e2 <- weakTerm
  return $ m :< WeakTermLet x e1 e2

-- let? x : A = e1 in e2
-- let? x     = e1 in e2
weakTermLetCoproduct :: Parser WeakTerm
weakTermLetCoproduct = do
  m <- currentHint
  try $ keyword "let?"
  x <- weakTermLetVar
  delimiter "="
  e1 <- weakTerm
  keyword "in"
  e2 <- weakTerm
  err <- liftIO $ newTextualIdentFromText "err"
  typeOfLeft <- liftIO $ newAster m
  typeOfRight <- liftIO $ newAster m
  let sumLeft = "sum.left"
  let sumRight = "sum.right"
  let sumLeftVar = Ident.fromText "sum.left"
  return $
    m
      :< WeakTermMatch
        Nothing
        (e1, doNotCare m)
        [ ( (m, sumLeft, [(m, err, typeOfLeft)]),
            m :< WeakTermPiElim (weakVar' m sumLeftVar) [typeOfLeft, typeOfRight, weakVar' m err]
          ),
          ( (m, sumRight, [x]),
            e2
          )
        ]

weakTermVoid :: Hint -> WeakTerm -> Parser WeakTerm
weakTermVoid m e1 = do
  delimiter ";"
  e2 <- weakTerm
  f <- liftIO $ newTextualIdentFromText "unit"
  return $ bind (m, f, m :< WeakTermEnum constTop) e1 e2

weakTermExplicitAscription :: Hint -> WeakTerm -> Parser WeakTerm
weakTermExplicitAscription m e = do
  delimiter ":"
  t <- weakTermEasy
  f <- liftIO $ newTextualIdentFromText "unit"
  return $ bind (m, f, t) e (m :< WeakTermVar f)

weakTermTau :: Parser WeakTerm
weakTermTau = do
  m <- currentHint
  try $ keyword "tau"
  return $ m :< WeakTermTau

weakTermAster :: Parser WeakTerm
weakTermAster = do
  m <- currentHint
  delimiter "?"
  liftIO $ newAster m

weakTermPi :: Parser WeakTerm
weakTermPi = do
  m <- currentHint
  domList <- argList $ choice [try weakAscription, typeWithoutIdent]
  delimiter "->"
  cod <- weakTerm
  return $ m :< WeakTermPi domList cod

weakTermPiIntro :: Parser WeakTerm
weakTermPiIntro = do
  m <- currentHint
  try $ keyword "lambda"
  varList <- argList weakBinder
  e <- weakTermDotBind <|> doBlock weakTerm
  return $ lam m varList e

weakTermDotBind :: Parser WeakTerm
weakTermDotBind = do
  delimiter "."
  weakTerm

parseDefInfo :: Parser DefInfo
parseDefInfo = do
  functionVar <- var
  domInfoList <- argList weakBinder
  delimiter ":"
  codType <- weakTerm
  e <- asBlock weakTerm
  return (functionVar, domInfoList, codType, e)

parseTopDefInfo :: Parser TopDefInfo
parseTopDefInfo = do
  functionVar <- var
  impDomInfoList <- impArgList weakBinder
  domInfoList <- argList weakBinder
  delimiter ":"
  codType <- weakTerm
  e <- asBlock weakTerm
  return (functionVar, impDomInfoList, domInfoList, codType, e)

-- define name(x1: A1, ..., xn: An)[: A] as e end
weakTermPiIntroDef :: Parser WeakTerm
weakTermPiIntroDef = do
  m <- currentHint
  try $ keyword "define"
  ((mFun, functionName), domBinderList, codType, e) <- parseDefInfo
  let piType = mFun :< WeakTermPi domBinderList codType
  return $ m :< WeakTermPiIntro (LamKindFix (mFun, Ident.fromText functionName, piType)) domBinderList e

weakTermSigma :: Parser WeakTerm
weakTermSigma = do
  m <- currentHint
  try $ keyword "tuple"
  ts <- argList $ choice [try weakAscription, typeWithoutIdent]
  return $ m :< WeakTermSigma ts

parseDefiniteDescription :: Parser (Hint, T.Text)
parseDefiniteDescription = do
  m <- currentHint
  x <- symbol
  delimiter definiteSep
  y <- symbol
  return (m, x <> definiteSep <> y)

weakTermDefiniteDescription :: Parser WeakTerm
weakTermDefiniteDescription = do
  (m, x) <- try parseDefiniteDescription
  return $ m :< WeakTermVarGlobal x

weakTermEnumElim :: Parser WeakTerm
weakTermEnumElim = do
  m <- currentHint
  try $ keyword "switch"
  e <- weakTerm
  keyword "with"
  clauseList <- many weakTermEnumClause
  keyword "end"
  h <- liftIO $ newAster m
  return $ m :< WeakTermEnumElim (e, h) clauseList

weakTermEnumClause :: Parser (EnumCase, WeakTerm)
weakTermEnumClause = do
  m <- currentHint
  delimiter "-"
  c <- symbol
  delimiter "->"
  body <- weakTerm
  case c of
    "default" ->
      return (m :< EnumCaseDefault, body)
    _ ->
      return (m :< EnumCaseLabel c, body)

-- question e
weakTermQuestion :: Parser WeakTerm
weakTermQuestion = do
  m <- currentHint
  try $ keyword "question"
  e <- weakTerm
  h <- liftIO $ newAster m
  return $ m :< WeakTermQuestion e h

weakTermMagic :: Parser WeakTerm
weakTermMagic = do
  m <- currentHint
  try $ keyword "magic"
  choice
    [ weakTermMagicCast m,
      weakTermMagicStore m,
      weakTermMagicLoad m,
      weakTermMagicSyscall m,
      weakTermMagicExternal m
    ]

weakTermMagicBase :: T.Text -> Parser WeakTerm -> Parser WeakTerm
weakTermMagicBase k parser = do
  keyword k
  betweenParen parser

weakTermMagicCast :: Hint -> Parser WeakTerm
weakTermMagicCast m = do
  weakTermMagicBase "cast" $ do
    castFrom <- weakTerm
    castTo <- delimiter "," >> weakTerm
    value <- delimiter "," >> weakTerm
    return $ m :< WeakTermMagic (MagicCast castFrom castTo value)

weakTermMagicStore :: Hint -> Parser WeakTerm
weakTermMagicStore m = do
  weakTermMagicBase "store" $ do
    lt <- lowType
    pointer <- delimiter "," >> weakTerm
    value <- delimiter "," >> weakTerm
    return $ m :< WeakTermMagic (MagicStore lt pointer value)

weakTermMagicLoad :: Hint -> Parser WeakTerm
weakTermMagicLoad m = do
  weakTermMagicBase "load" $ do
    lt <- lowType
    pointer <- delimiter "," >> weakTerm
    return $ m :< WeakTermMagic (MagicLoad lt pointer)

weakTermMagicSyscall :: Hint -> Parser WeakTerm
weakTermMagicSyscall m = do
  weakTermMagicBase "syscall" $ do
    syscallNum <- integer
    es <- many (delimiter "," >> weakTerm)
    return $ m :< WeakTermMagic (MagicSyscall syscallNum es)

weakTermMagicExternal :: Hint -> Parser WeakTerm
weakTermMagicExternal m = do
  weakTermMagicBase "external" $ do
    extFunName <- symbol
    es <- many (delimiter "," >> weakTerm)
    return $ m :< WeakTermMagic (MagicExternal extFunName es)

-- -- t ::= i{n} | f{n} | pointer t | array INT t | struct t ... t
lowType :: Parser LowType
lowType = do
  choice
    [ lowTypePointer,
      lowTypeArray,
      lowTypeStruct,
      lowTypeSimple
    ]

lowTypeSimple :: Parser LowType
lowTypeSimple =
  choice
    [ betweenParen lowType,
      lowTypeNumber
    ]

lowTypePointer :: Parser LowType
lowTypePointer = do
  keyword "pointer"
  lowTypeSimple

lowTypeArray :: Parser LowType
lowTypeArray = do
  keyword "array"
  intValue <- integer
  LowTypeArray (fromInteger intValue) <$> lowTypeSimple

lowTypeStruct :: Parser LowType
lowTypeStruct = do
  keyword "struct"
  LowTypeStruct <$> many lowTypeSimple

lowTypeNumber :: Parser LowType
lowTypeNumber = do
  sizeString <- symbol
  case fromText sizeString of
    Just primNum ->
      return $ LowTypePrimNum primNum
    _ ->
      failure (Just (asTokens sizeString)) (S.fromList [asLabel "i{n}", asLabel "f{n}"])

weakTermMatch :: Parser WeakTerm
weakTermMatch = do
  m <- currentHint
  try $ keyword "match"
  e <- weakTerm
  clauseList <- withBlock $ manyList weakTermMatchClause
  return $ m :< WeakTermMatch Nothing (e, doNotCare m) clauseList

weakTermMatchNoetic :: Parser WeakTerm
weakTermMatchNoetic = do
  m <- currentHint
  try $ keyword "match-noetic"
  e <- weakTerm
  keyword "with"
  s <- liftIO $ newAster m
  t <- liftIO $ newAster m
  let e' = castFromNoema s t e
  clauseList <- manyList weakTermMatchClause
  keyword "end"
  let clauseList' = map (modifyWeakPattern s) clauseList
  return $ m :< WeakTermMatch (Just s) (e', doNotCare m) clauseList'

weakTermMatchClause :: Parser (PatternF WeakTerm, WeakTerm)
weakTermMatchClause = do
  pat <- weakTermPattern
  delimiter "->"
  body <- weakTerm
  return (pat, body)

modifyWeakPattern :: WeakTerm -> (PatternF WeakTerm, WeakTerm) -> (PatternF WeakTerm, WeakTerm)
modifyWeakPattern s ((m, a, xts), body) =
  ((m, a, xts), modifyWeakPatternBody s xts body)

modifyWeakPatternBody :: WeakTerm -> [BinderF WeakTerm] -> WeakTerm -> WeakTerm
modifyWeakPatternBody s xts body =
  case xts of
    [] ->
      body
    ((m, x, t) : rest) ->
      bind (m, x, wrapWithNoema s t) (castToNoema s t (weakVar' m x)) $
        modifyWeakPatternBody s rest body

weakTermPattern :: Parser (PatternF WeakTerm)
weakTermPattern = do
  m <- currentHint
  c <- symbol
  patArgs <- argList weakBinder
  return (m, c, patArgs)

-- let (x1 : A1, ..., xn : An) = e1 in e2
weakTermLetSigmaElim :: Parser WeakTerm
weakTermLetSigmaElim = do
  m <- currentHint
  try $ keyword "let"
  -- xts <- parseArgList2 weakBinder
  xts <- argList weakBinder
  delimiter "="
  e1 <- weakTerm
  keyword "in"
  e2 <- weakTerm
  return $ m :< WeakTermSigmaElim xts e1 e2

weakTermLetVar :: Parser (BinderF WeakTerm)
weakTermLetVar = do
  m <- currentHint
  choice
    [ try $ do
        x <- symbol
        delimiter ":"
        a <- weakTerm
        return (m, Ident.fromText x, a),
      do
        x <- symbol
        h <- liftIO $ newAster m
        return (m, Ident.fromText x, h)
    ]

weakTermIf :: Parser WeakTerm
weakTermIf = do
  m <- currentHint
  try $ keyword "if"
  ifCond <- weakTerm
  keyword "then"
  ifBody <- weakTerm
  elseIfList <- many $ do
    keyword "else-if"
    elseIfCond <- weakTerm
    keyword "then"
    elseIfBody <- weakTerm
    return (elseIfCond, elseIfBody)
  keyword "else"
  elseBody <- weakTerm
  keyword "end"
  liftIO $ foldIf m ifCond ifBody elseIfList elseBody

foldIf :: Hint -> WeakTerm -> WeakTerm -> [(WeakTerm, WeakTerm)] -> WeakTerm -> IO WeakTerm
foldIf m ifCond ifBody elseIfList elseBody =
  case elseIfList of
    [] -> do
      h <- newAster m
      return $
        m
          :< WeakTermEnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel constBoolTrue, ifBody),
              (m :< EnumCaseLabel constBoolFalse, elseBody)
            ]
    ((elseIfCond, elseIfBody) : rest) -> do
      cont <- foldIf m elseIfCond elseIfBody rest elseBody
      h <- newAster m
      return $
        m
          :< WeakTermEnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel constBoolTrue, ifBody),
              (m :< EnumCaseLabel constBoolFalse, cont)
            ]

weakTermParen :: Parser WeakTerm
weakTermParen = do
  m <- currentHint
  es <- argList weakTerm
  case es of
    [] ->
      return $ m :< WeakTermSigmaIntro []
    [e] ->
      return e
    _ ->
      return $ m :< WeakTermSigmaIntro es

-- -- (e1, ..., en) (n >= 2)
weakTermSigmaIntro :: Parser WeakTerm
weakTermSigmaIntro = do
  m <- currentHint
  try $ keyword "tuple-new"
  es <- argList weakTerm
  return $ m :< WeakTermSigmaIntro es

weakTermNoema :: Parser WeakTerm
weakTermNoema = do
  m <- currentHint
  try $ delimiter "&"
  subject <- Ident.fromText <$> symbol
  t <- weakTerm
  return $ m :< WeakTermNoema (m :< WeakTermVar subject) t

weakTermIdealize :: Parser WeakTerm
weakTermIdealize = do
  m <- currentHint
  try $ keyword "idealize"
  varList <- manyTill var (keyword "over")
  let varList' = fmap (fmap Ident.fromText) varList
  subject <- Ident.fromText <$> symbol
  e <- doBlock weakTerm
  ts <- liftIO $ mapM (\(mx, _) -> newAster mx) varList
  return $ m :< WeakTermNoemaElim subject (castLet subject (zip varList' ts) e)

castLet :: Ident -> [((Hint, Ident), WeakTerm)] -> WeakTerm -> WeakTerm
castLet subject xts cont =
  case xts of
    [] ->
      cont
    ((m, x), t) : rest ->
      bind (m, x, t) (m :< WeakTermNoemaIntro subject (weakVar' m x)) $ castLet subject rest cont

weakTermArray :: Parser WeakTerm
weakTermArray = do
  m <- currentHint
  try $ keyword "array"
  betweenParen $ do
    elemType <- weakTerm
    return $ m :< WeakTermArray elemType

weakTermArrayIntro :: Parser WeakTerm
weakTermArrayIntro = do
  m <- currentHint
  try $ keyword "array-new"
  betweenParen $ do
    elems <- sepBy weakTerm (delimiter ",")
    return $ m :< WeakTermArrayIntro (doNotCare m) elems

weakTermArrayAccess :: Parser WeakTerm
weakTermArrayAccess = do
  m <- currentHint
  try $ keyword "array-access"
  betweenParen $ do
    array <- weakTerm
    delimiter ","
    index <- weakTerm
    return $ m :< WeakTermArrayAccess (doNotCare m) (doNotCare m) array index

weakTermCell :: Parser WeakTerm
weakTermCell = do
  m <- currentHint
  try $ keyword "cell"
  betweenParen $ do
    contentType <- weakTerm
    return $ m :< WeakTermCell contentType

weakTermCellIntro :: Parser WeakTerm
weakTermCellIntro = do
  m <- currentHint
  try $ keyword "cell-new"
  betweenParen $ do
    content <- weakTerm
    return $ m :< WeakTermCellIntro (doNotCare m) content

weakTermCellRead :: Parser WeakTerm
weakTermCellRead = do
  m <- currentHint
  try $ keyword "cell-read"
  betweenParen $ do
    cell <- weakTerm
    return $ m :< WeakTermCellRead cell

weakTermCellWrite :: Parser WeakTerm
weakTermCellWrite = do
  m <- currentHint
  try $ keyword "cell-write"
  betweenParen $ do
    cell <- weakTerm
    delimiter ","
    newValue <- weakTerm
    return $ m :< WeakTermCellWrite cell newValue

bind :: BinderF WeakTerm -> WeakTerm -> WeakTerm -> WeakTerm
bind mxt@(m, _, _) e cont =
  m :< WeakTermLet mxt e cont

weakTermAdmit :: Parser WeakTerm
weakTermAdmit = do
  m <- currentHint
  try $ keyword "admit"
  h <- liftIO $ newAster m
  return $
    m
      :< WeakTermPiElim
        (weakVar m "core.os.exit")
        [ h,
          m :< WeakTermInt (m :< WeakTermConst "i64") 1
        ]

weakTermAdmitQuestion :: Parser WeakTerm
weakTermAdmitQuestion = do
  m <- currentHint
  try $ keyword "?admit"
  h <- liftIO $ newAster m
  return $
    m
      :< WeakTermQuestion
        ( m
            :< WeakTermPiElim
              (weakVar m "os.exit")
              [ h,
                m :< WeakTermInt (m :< WeakTermConst "i64") 1
              ]
        )
        h

weakTermPiElim :: Parser WeakTerm
weakTermPiElim = do
  m <- currentHint
  e <- weakTermSimple
  impArgs <- impArgList weakTerm
  es <- argList weakTerm
  ess <- many $ argList weakTerm
  if null impArgs
    then return $ foldl' (\base args -> m :< WeakTermPiElim base args) e $ es : ess
    else do
      f <- liftIO $ newTextualIdentFromText "func"
      h <- liftIO $ newAster m
      return $
        m
          :< WeakTermLet
            (m, f, h)
            e
            ( foldl'
                (\base args -> m :< WeakTermPiElim base args)
                (m :< WeakTermVar f)
                ((impArgs ++ es) : ess)
            )

weakTermPiElimInv :: Parser WeakTerm
weakTermPiElimInv = do
  m <- currentHint
  e <- weakTermSimple
  f <- betweenBracket weakTerm
  fs <- many $ betweenBracket weakTerm
  return $ foldl' (\base func -> m :< WeakTermPiElim func [base]) e $ f : fs

-- --
-- -- term-related helper functions
-- --

weakBinder :: Parser (BinderF WeakTerm)
weakBinder =
  choice
    [ try weakAscription,
      weakAscription'
    ]

weakAscription :: Parser (BinderF WeakTerm)
weakAscription = do
  m <- currentHint
  x <- symbol
  delimiter ":"
  a <- weakTerm
  return (m, Ident.fromText x, a)

typeWithoutIdent :: Parser (BinderF WeakTerm)
typeWithoutIdent = do
  m <- currentHint
  x <- liftIO $ newTextualIdentFromText "_"
  t <- weakTerm
  return (m, x, t)

weakAscription' :: Parser (BinderF WeakTerm)
weakAscription' = do
  (m, x) <- weakSimpleIdent
  h <- liftIO $ newAster m
  return (m, x, h)

weakSimpleIdent :: Parser (Hint, Ident)
weakSimpleIdent = do
  m <- currentHint
  x <- symbol
  return (m, Ident.fromText x)

weakTermIntrospect :: Parser WeakTerm
weakTermIntrospect = do
  m <- currentHint
  try $ keyword "introspect"
  key <- symbol
  value <- liftIO $ getIntrospectiveValue m key
  keyword "with"
  clauseList <- many weakTermIntrospectiveClause
  keyword "end"
  case lookup value clauseList of
    Just clause ->
      return clause
    Nothing -> do
      liftIO $ putStrLn "weakTermIntrospect (not implemented)"
      undefined

-- liftIO $ outputError m $ "`" <> value <> "` is not supported here"

weakTermIntrospectiveClause :: Parser (T.Text, WeakTerm)
weakTermIntrospectiveClause = do
  delimiter "-"
  c <- symbol
  delimiter "->"
  body <- weakTerm
  return (c, body)

getIntrospectiveValue :: Hint -> T.Text -> IO T.Text
getIntrospectiveValue m key =
  case key of
    "target-platform" -> do
      T.pack <$> readIORef targetPlatformRef
    "target-arch" ->
      T.pack <$> readIORef targetArchRef
    "target-os" ->
      T.pack <$> readIORef targetOSRef
    _ ->
      raiseError m $ "no such introspective value is defined: " <> key

weakTermVar :: Parser WeakTerm
weakTermVar = do
  (m, x) <- var
  return (weakVar m x)

weakTermText :: Parser WeakTerm
weakTermText = do
  m <- currentHint
  try $ keyword "text"
  return $ m :< WeakTermText

weakTermTextIntro :: Parser WeakTerm
weakTermTextIntro = do
  m <- currentHint
  s <- string
  return $ m :< WeakTermTextIntro s

weakTermInteger :: Parser WeakTerm
weakTermInteger = do
  m <- currentHint
  intValue <- try integer
  h <- liftIO $ newAster m
  return $ m :< WeakTermInt h intValue

weakTermFloat :: Parser WeakTerm
weakTermFloat = do
  m <- currentHint
  floatValue <- try float
  h <- liftIO $ newAster m
  return $ m :< WeakTermFloat h floatValue

castFromNoema :: WeakTerm -> WeakTerm -> WeakTerm -> WeakTerm
castFromNoema subject baseType tree = do
  let m = metaOf tree
  m :< WeakTermMagic (MagicCast (wrapWithNoema subject baseType) baseType tree)

castToNoema :: WeakTerm -> WeakTerm -> WeakTerm -> WeakTerm
castToNoema subject baseType tree = do
  let m = metaOf tree
  m :< WeakTermMagic (MagicCast baseType (wrapWithNoema subject baseType) tree)

wrapWithNoema :: WeakTerm -> WeakTerm -> WeakTerm
wrapWithNoema subject baseType = do
  let m = metaOf baseType
  m :< WeakTermNoema subject baseType

doNotCare :: Hint -> WeakTerm
doNotCare m =
  m :< WeakTermTau

lam :: Hint -> [BinderF WeakTerm] -> WeakTerm -> WeakTerm
lam m varList e =
  m :< WeakTermPiIntro LamKindNormal varList e

weakVar :: Hint -> T.Text -> WeakTerm
weakVar m str =
  m :< WeakTermVar (Ident.fromText str)

weakVar' :: Hint -> Ident -> WeakTerm
weakVar' m ident =
  m :< WeakTermVar ident

newTextualIdentFromText :: T.Text -> IO Ident
newTextualIdentFromText txt = do
  i <- newCount
  newIdentFromText $ ";" <> txt <> T.pack (show i)
