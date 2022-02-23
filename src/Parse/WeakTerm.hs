module Parse.WeakTerm
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

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad.IO.Class
import Data.Basic
  ( BinderF,
    EnumCase,
    EnumCaseF (EnumCaseDefault, EnumCaseLabel),
    Hint,
    Ident,
    LamKindF (LamKindFix, LamKindNormal),
    PatternF,
    asIdent,
  )
import Data.Global (constBoolFalse, constBoolTrue, definiteSep, newAster, newCount, newIdentFromText, outputError, targetArchRef, targetOSRef, targetPlatformRef)
import Data.IORef (readIORef)
import Data.List (foldl')
import Data.Log (raiseError)
import Data.LowType
  ( LowType (..),
    Magic (..),
    PrimNum (PrimNumFloat, PrimNumInt),
    asLowFloat,
    asLowInt,
  )
import qualified Data.Set as S
import qualified Data.Text as T
import Data.WeakTerm
  ( DefInfo,
    TopDefInfo,
    WeakTerm,
    WeakTermF (..),
    metaOf,
  )
import Parse.Core
  ( Parser,
    argList,
    asBlock,
    asLabel,
    asTokens,
    betweenBracket,
    betweenParen,
    currentHint,
    delimiter,
    doBlock,
    float,
    impArgList,
    integer,
    keyword,
    manyList,
    string,
    symbol,
    var,
    withBlock,
  )
import Text.Megaparsec (choice, chunk, failure, many, manyTill, sepBy, try, (<|>))

--
-- parser for WeakTerm
--

weakTerm :: Parser WeakTerm
weakTerm = do
  choice
    [ try weakTermPiIntro,
      try weakTermPiIntroDef,
      try weakTermAster,
      try weakTermEnumElim,
      try weakTermIntrospect,
      try weakTermQuestion,
      try weakTermMagic,
      try weakTermMatch,
      try weakTermMatchNoetic,
      try weakTermLet,
      try weakTermLetCoproduct,
      try weakTermIf,
      weakTermIdealize,
      try weakTermArray,
      try weakTermArrayIntro,
      try weakTermArrayAccess,
      try weakTermText,
      try weakTermCell,
      try weakTermCellIntro,
      try weakTermCellRead,
      try weakTermCellWrite,
      try weakTermNoema,
      try weakTermSigma,
      try weakTermPi,
      try weakTermPiElim,
      try weakTermPiElimInv,
      weakTermSimple
    ]

weakTermSimple :: Parser WeakTerm
weakTermSimple = do
  choice
    [ try weakTermSigmaIntro,
      try $ betweenParen weakTerm,
      try weakTermTau,
      try weakTermAster,
      try weakTermTextIntro,
      try weakTermAdmitQuestion,
      try weakTermAdmit,
      try weakTermInteger,
      try weakTermFloat,
      try weakTermDefiniteDescription,
      weakTermVar
    ]

weakTermTau :: Parser WeakTerm
weakTermTau = do
  m <- currentHint
  keyword "tau"
  return $ m :< WeakTermTau

weakTermAster :: Parser WeakTerm
weakTermAster = do
  m <- currentHint
  keyword "?"
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
  keyword "lambda"
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
  keyword "define"
  ((mFun, functionName), domBinderList, codType, e) <- parseDefInfo
  let piType = mFun :< WeakTermPi domBinderList codType
  return $ m :< WeakTermPiIntro (LamKindFix (mFun, asIdent functionName, piType)) domBinderList e

weakTermSigma :: Parser WeakTerm
weakTermSigma = do
  m <- currentHint
  keyword "tuple"
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
  (m, x) <- parseDefiniteDescription
  return $ m :< WeakTermVarGlobal x

weakTermEnumElim :: Parser WeakTerm
weakTermEnumElim = do
  m <- currentHint
  keyword "switch"
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

-- -- question e
weakTermQuestion :: Parser WeakTerm
weakTermQuestion = do
  m <- currentHint
  keyword "question"
  e <- weakTerm
  h <- liftIO $ newAster m
  return $ m :< WeakTermQuestion e h

weakTermMagic :: Parser WeakTerm
weakTermMagic = do
  m <- currentHint
  keyword "magic"
  choice
    [ weakTermMagicCast m,
      weakTermMagicStore m,
      weakTermMagicLoad m,
      weakTermMagicSyscall m,
      weakTermMagicExternal m
    ]

weakTermMagicBase :: T.Text -> Parser WeakTerm -> Parser WeakTerm
weakTermMagicBase k p = do
  keyword k
  betweenParen p

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
  case (asLowInt sizeString, asLowFloat sizeString) of
    (Just size, _) ->
      return $ LowTypePrimNum $ PrimNumInt size
    (_, Just size) ->
      return $ LowTypePrimNum $ PrimNumFloat size
    _ ->
      failure (Just (asTokens sizeString)) (S.fromList [asLabel "i{n}", asLabel "f{n}"])

weakTermMatch :: Parser WeakTerm
weakTermMatch = do
  m <- currentHint
  keyword "match"
  e <- weakTerm
  clauseList <- withBlock $ manyList weakTermMatchClause
  return $ m :< WeakTermMatch Nothing (e, doNotCare m) clauseList

weakTermMatchNoetic :: Parser WeakTerm
weakTermMatchNoetic = do
  m <- currentHint
  keyword "match-noetic"
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

-- -- let x : A = e1 in e2
-- -- let x     = e1 in e2
weakTermLet :: Parser WeakTerm
weakTermLet = do
  choice [try weakTermLetSigmaElim, weakTermLetNormal]

weakTermLetNormal :: Parser WeakTerm
weakTermLetNormal = do
  m <- currentHint
  keyword "let"
  x <- weakTermLetVar
  delimiter "="
  e1 <- weakTerm
  keyword "in"
  e2 <- weakTerm
  t1 <- liftIO $ newAster m
  resultType <- liftIO $ newAster m
  return $
    m
      :< WeakTermPiElim
        (m :< WeakTermVarGlobal "core.identity::bind")
        [ t1,
          resultType,
          e1,
          lam m [x] e2
        ]

-- let (x1 : A1, ..., xn : An) = e1 in e2
weakTermLetSigmaElim :: Parser WeakTerm
weakTermLetSigmaElim = do
  m <- currentHint
  keyword "let"
  -- xts <- parseArgList2 weakBinder
  xts <- argList weakBinder
  delimiter "="
  e1 <- weakTerm
  keyword "in"
  e2 <- weakTerm
  return $ m :< WeakTermSigmaElim xts e1 e2

-- let? x : A = e1 in e2
-- let? x     = e1 in e2
weakTermLetCoproduct :: Parser WeakTerm
weakTermLetCoproduct = do
  m <- currentHint
  keyword "let?"
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
  let sumLeftVar = asIdent "sum.left"
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

weakTermLetVar :: Parser (BinderF WeakTerm)
weakTermLetVar = do
  m <- currentHint
  choice
    [ try $ do
        x <- symbol
        delimiter ":"
        a <- weakTerm
        return (m, asIdent x, a),
      do
        x <- symbol
        h <- liftIO $ newAster m
        return (m, asIdent x, h)
    ]

weakTermIf :: Parser WeakTerm
weakTermIf = do
  m <- currentHint
  keyword "if"
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

-- -- (e1, ..., en) (n >= 2)
weakTermSigmaIntro :: Parser WeakTerm
weakTermSigmaIntro = do
  m <- currentHint
  es <- argList weakTerm
  if length es > 2
    then return $ m :< WeakTermSigmaIntro es
    else undefined

weakTermNoema :: Parser WeakTerm
weakTermNoema = do
  m <- currentHint
  _ <- chunk "&"
  subject <- asIdent <$> symbol
  t <- weakTerm
  return $ m :< WeakTermNoema (m :< WeakTermVar subject) t

weakTermIdealize :: Parser WeakTerm
weakTermIdealize = do
  m <- currentHint
  try $ keyword "idealize"
  varList <- manyTill var (keyword "over")
  let varList' = fmap (fmap asIdent) varList
  subject <- asIdent <$> symbol
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
  keyword "array"
  betweenParen $ do
    elemType <- weakTerm
    return $ m :< WeakTermArray elemType

weakTermArrayIntro :: Parser WeakTerm
weakTermArrayIntro = do
  m <- currentHint
  keyword "array-new"
  betweenParen $ do
    elems <- sepBy weakTerm (delimiter ",")
    return $ m :< WeakTermArrayIntro (doNotCare m) elems

weakTermArrayAccess :: Parser WeakTerm
weakTermArrayAccess = do
  m <- currentHint
  keyword "array-access"
  betweenParen $ do
    array <- weakTerm
    delimiter ","
    index <- weakTerm
    return $ m :< WeakTermArrayAccess (doNotCare m) (doNotCare m) array index

weakTermCell :: Parser WeakTerm
weakTermCell = do
  m <- currentHint
  keyword "cell"
  betweenParen $ do
    contentType <- weakTerm
    return $ m :< WeakTermCell contentType

weakTermCellIntro :: Parser WeakTerm
weakTermCellIntro = do
  m <- currentHint
  keyword "cell-new"
  betweenParen $ do
    content <- weakTerm
    return $ m :< WeakTermCellIntro (doNotCare m) content

weakTermCellRead :: Parser WeakTerm
weakTermCellRead = do
  m <- currentHint
  keyword "cell-read"
  betweenParen $ do
    cell <- weakTerm
    return $ m :< WeakTermCellRead cell

weakTermCellWrite :: Parser WeakTerm
weakTermCellWrite = do
  m <- currentHint
  keyword "cell-write"
  betweenParen $ do
    cell <- weakTerm
    delimiter ","
    newValue <- weakTerm
    return $ m :< WeakTermCellWrite cell newValue

bind :: BinderF WeakTerm -> WeakTerm -> WeakTerm -> WeakTerm
bind mxt@(m, _, _) e cont =
  m :< WeakTermPiElim (lam m [mxt] cont) [e]

weakTermAdmit :: Parser WeakTerm
weakTermAdmit = do
  m <- currentHint
  keyword "admit"
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
  keyword "?admit"
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
          :< WeakTermPiElim
            ( m
                :< WeakTermPiIntro
                  LamKindNormal
                  [(m, f, h)]
                  ( foldl'
                      (\base args -> m :< WeakTermPiElim base args)
                      (m :< WeakTermVar f)
                      ((impArgs ++ es) : ess)
                  )
            )
            [e]

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
  return (m, asIdent x, a)

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
  return (m, asIdent x)

weakTermIntrospect :: Parser WeakTerm
weakTermIntrospect = do
  m <- currentHint
  keyword "introspect"
  key <- symbol
  value <- liftIO $ getIntrospectiveValue m key
  keyword "with"
  clauseList <- many weakTermIntrospectiveClause
  keyword "end"
  case lookup value clauseList of
    Just clause ->
      return clause
    Nothing -> do
      liftIO $ outputError m $ "`" <> value <> "` is not supported here"

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
  keyword "text"
  return $ m :< WeakTermText

weakTermTextIntro :: Parser WeakTerm
weakTermTextIntro = do
  m <- currentHint
  s <- string
  return $ m :< WeakTermTextIntro s

weakTermInteger :: Parser WeakTerm
weakTermInteger = do
  m <- currentHint
  intValue <- integer
  h <- liftIO $ newAster m
  return $ m :< WeakTermInt h intValue

weakTermFloat :: Parser WeakTerm
weakTermFloat = do
  m <- currentHint
  floatValue <- float
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
  m :< WeakTermVar (asIdent str)

weakVar' :: Hint -> Ident -> WeakTerm
weakVar' m ident =
  m :< WeakTermVar ident

newTextualIdentFromText :: T.Text -> IO Ident
newTextualIdentFromText txt = do
  i <- newCount
  newIdentFromText $ ";" <> txt <> T.pack (show i)
