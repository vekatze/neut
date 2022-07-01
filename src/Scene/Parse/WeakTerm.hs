module Scene.Parse.WeakTerm
  ( weakTerm,
    weakTermSimple,
    weakBinder,
    weakAscription,
    parseTopDefInfo,
    parseDefiniteDescription,
    weakVar,
  )
where

import Context.App
import qualified Context.Gensym as Gensym
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import Control.Monad.IO.Class
import Data.Function
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

weakTerm :: Axis -> Parser WeakTerm
weakTerm axis = do
  m <- currentHint
  e1 <- weakTermEasy axis
  choice
    [ weakTermVoid axis m e1,
      weakTermExplicitAscription axis m e1,
      return e1
    ]

-- fixme: easy??
weakTermEasy :: Axis -> Parser WeakTerm
weakTermEasy axis = do
  choice
    [ weakTermPiIntro axis,
      weakTermPiIntroDef axis,
      weakTermSigma axis,
      weakTermSigmaIntro axis,
      weakTermEnumElim axis,
      weakTermIntrospect axis,
      weakTermQuestion axis,
      weakTermMagic axis,
      weakTermMatch axis,
      weakTermMatchNoetic axis,
      weakTermIf axis,
      weakTermIdealize axis,
      weakTermArray axis,
      weakTermArrayIntro axis,
      weakTermArrayAccess axis,
      weakTermText,
      weakTermCell axis,
      weakTermCellIntro axis,
      weakTermCellRead axis,
      weakTermCellWrite axis,
      weakTermNoema axis,
      try $ weakTermLetSigmaElim axis,
      weakTermLet axis,
      weakTermLetCoproduct axis,
      try $ weakTermPi axis,
      try $ weakTermPiElim axis,
      try $ weakTermPiElimInv axis,
      weakTermSimple axis
    ]

weakTermSimple :: Axis -> Parser WeakTerm
weakTermSimple axis = do
  choice
    [ weakTermParen axis,
      weakTermTau,
      weakTermTextIntro,
      weakTermAdmitQuestion (axis & gensym),
      weakTermAdmit (axis & gensym),
      weakTermAster axis,
      weakTermInteger (axis & gensym),
      weakTermFloat (axis & gensym),
      weakTermDefiniteDescription,
      weakTermVar
    ]

weakTermLet :: Axis -> Parser WeakTerm
weakTermLet axis = do
  m <- currentHint
  try $ keyword "let"
  x <- weakTermLetVar axis
  delimiter "="
  e1 <- weakTerm axis
  keyword "in"
  e2 <- weakTerm axis
  return $ m :< WeakTermLet x e1 e2

-- let? x : A = e1 in e2
-- let? x     = e1 in e2
weakTermLetCoproduct :: Axis -> Parser WeakTerm
weakTermLetCoproduct axis = do
  m <- currentHint
  try $ keyword "let?"
  x <- weakTermLetVar axis
  delimiter "="
  e1 <- weakTerm axis
  keyword "in"
  e2 <- weakTerm axis
  err <- liftIO $ Gensym.newTextualIdentFromText (axis & gensym) "err"
  typeOfLeft <- liftIO $ Gensym.newAster (gensym axis) m
  typeOfRight <- liftIO $ Gensym.newAster (gensym axis) m
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

weakTermVoid :: Axis -> Hint -> WeakTerm -> Parser WeakTerm
weakTermVoid axis m e1 = do
  delimiter ";"
  e2 <- weakTerm axis
  f <- liftIO $ Gensym.newTextualIdentFromText (axis & gensym) "unit"
  return $ bind (m, f, m :< WeakTermEnum constTop) e1 e2

weakTermExplicitAscription :: Axis -> Hint -> WeakTerm -> Parser WeakTerm
weakTermExplicitAscription axis m e = do
  delimiter ":"
  t <- weakTermEasy axis
  f <- liftIO $ Gensym.newTextualIdentFromText (axis & gensym) "unit"
  return $ bind (m, f, t) e (m :< WeakTermVar f)

weakTermTau :: Parser WeakTerm
weakTermTau = do
  m <- currentHint
  try $ keyword "tau"
  return $ m :< WeakTermTau

weakTermAster :: Axis -> Parser WeakTerm
weakTermAster axis = do
  m <- currentHint
  delimiter "?"
  liftIO $ Gensym.newAster (gensym axis) m

weakTermPi :: Axis -> Parser WeakTerm
weakTermPi axis = do
  m <- currentHint
  domList <- argList $ choice [try (weakAscription axis), typeWithoutIdent axis]
  delimiter "->"
  cod <- weakTerm axis
  return $ m :< WeakTermPi domList cod

weakTermPiIntro :: Axis -> Parser WeakTerm
weakTermPiIntro axis = do
  m <- currentHint
  try $ keyword "lambda"
  varList <- argList $ weakBinder axis
  e <- weakTermDotBind axis <|> doBlock (weakTerm axis)
  return $ lam m varList e

weakTermDotBind :: Axis -> Parser WeakTerm
weakTermDotBind axis = do
  delimiter "."
  weakTerm axis

parseDefInfo :: Axis -> Parser DefInfo
parseDefInfo axis = do
  functionVar <- var
  domInfoList <- argList $ weakBinder axis
  delimiter ":"
  codType <- weakTerm axis
  e <- asBlock (weakTerm axis)
  return (functionVar, domInfoList, codType, e)

parseTopDefInfo :: Axis -> Parser TopDefInfo
parseTopDefInfo axis = do
  functionVar <- var
  impDomInfoList <- impArgList $ weakBinder axis
  domInfoList <- argList $ weakBinder axis
  delimiter ":"
  codType <- weakTerm axis
  e <- asBlock (weakTerm axis)
  return (functionVar, impDomInfoList, domInfoList, codType, e)

-- define name(x1: A1, ..., xn: An)[: A] as e end
weakTermPiIntroDef :: Axis -> Parser WeakTerm
weakTermPiIntroDef axis = do
  m <- currentHint
  try $ keyword "define"
  ((mFun, functionName), domBinderList, codType, e) <- parseDefInfo axis
  let piType = mFun :< WeakTermPi domBinderList codType
  return $ m :< WeakTermPiIntro (LamKindFix (mFun, Ident.fromText functionName, piType)) domBinderList e

weakTermSigma :: Axis -> Parser WeakTerm
weakTermSigma axis = do
  m <- currentHint
  try $ keyword "tuple"
  ts <- argList $ choice [try $ weakAscription axis, typeWithoutIdent axis]
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

weakTermEnumElim :: Axis -> Parser WeakTerm
weakTermEnumElim axis = do
  m <- currentHint
  try $ keyword "switch"
  e <- weakTerm axis
  keyword "with"
  clauseList <- many (weakTermEnumClause axis)
  keyword "end"
  h <- liftIO $ Gensym.newAster (gensym axis) m
  return $ m :< WeakTermEnumElim (e, h) clauseList

weakTermEnumClause :: Axis -> Parser (EnumCase, WeakTerm)
weakTermEnumClause axis = do
  m <- currentHint
  delimiter "-"
  c <- symbol
  delimiter "->"
  body <- weakTerm axis
  let dummyLabelInfo = ("", 0)
  case c of
    "default" ->
      return (m :< EnumCaseDefault, body)
    _ ->
      return (m :< EnumCaseLabel dummyLabelInfo c, body)

-- question e
weakTermQuestion :: Axis -> Parser WeakTerm
weakTermQuestion axis = do
  m <- currentHint
  try $ keyword "question"
  e <- weakTerm axis
  h <- liftIO $ Gensym.newAster (gensym axis) m
  return $ m :< WeakTermQuestion e h

weakTermMagic :: Axis -> Parser WeakTerm
weakTermMagic axis = do
  m <- currentHint
  try $ keyword "magic"
  choice
    [ weakTermMagicCast axis m,
      weakTermMagicStore axis m,
      weakTermMagicLoad axis m,
      weakTermMagicSyscall axis m,
      weakTermMagicExternal axis m
    ]

weakTermMagicBase :: T.Text -> Parser WeakTerm -> Parser WeakTerm
weakTermMagicBase k parser = do
  keyword k
  betweenParen parser

weakTermMagicCast :: Axis -> Hint -> Parser WeakTerm
weakTermMagicCast axis m = do
  weakTermMagicBase "cast" $ do
    castFrom <- weakTerm axis
    castTo <- delimiter "," >> weakTerm axis
    value <- delimiter "," >> weakTerm axis
    return $ m :< WeakTermMagic (MagicCast castFrom castTo value)

weakTermMagicStore :: Axis -> Hint -> Parser WeakTerm
weakTermMagicStore axis m = do
  weakTermMagicBase "store" $ do
    lt <- lowType
    pointer <- delimiter "," >> weakTerm axis
    value <- delimiter "," >> weakTerm axis
    return $ m :< WeakTermMagic (MagicStore lt pointer value)

weakTermMagicLoad :: Axis -> Hint -> Parser WeakTerm
weakTermMagicLoad axis m = do
  weakTermMagicBase "load" $ do
    lt <- lowType
    pointer <- delimiter "," >> weakTerm axis
    return $ m :< WeakTermMagic (MagicLoad lt pointer)

weakTermMagicSyscall :: Axis -> Hint -> Parser WeakTerm
weakTermMagicSyscall axis m = do
  weakTermMagicBase "syscall" $ do
    syscallNum <- integer
    es <- many (delimiter "," >> weakTerm axis)
    return $ m :< WeakTermMagic (MagicSyscall syscallNum es)

weakTermMagicExternal :: Axis -> Hint -> Parser WeakTerm
weakTermMagicExternal axis m = do
  weakTermMagicBase "external" $ do
    extFunName <- symbol
    es <- many (delimiter "," >> weakTerm axis)
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

weakTermMatch :: Axis -> Parser WeakTerm
weakTermMatch axis = do
  m <- currentHint
  try $ keyword "match"
  e <- weakTerm axis
  clauseList <- withBlock $ manyList $ weakTermMatchClause axis
  return $ m :< WeakTermMatch Nothing (e, doNotCare m) clauseList

weakTermMatchNoetic :: Axis -> Parser WeakTerm
weakTermMatchNoetic axis = do
  m <- currentHint
  try $ keyword "match-noetic"
  e <- weakTerm axis
  keyword "with"
  s <- liftIO $ Gensym.newAster (gensym axis) m
  t <- liftIO $ Gensym.newAster (gensym axis) m
  let e' = castFromNoema s t e
  clauseList <- manyList $ weakTermMatchClause axis
  keyword "end"
  let clauseList' = map (modifyWeakPattern s) clauseList
  return $ m :< WeakTermMatch (Just s) (e', doNotCare m) clauseList'

weakTermMatchClause :: Axis -> Parser (PatternF WeakTerm, WeakTerm)
weakTermMatchClause axis = do
  pat <- weakTermPattern axis
  delimiter "->"
  body <- weakTerm axis
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

weakTermPattern :: Axis -> Parser (PatternF WeakTerm)
weakTermPattern axis = do
  m <- currentHint
  c <- symbol
  patArgs <- argList $ weakBinder axis
  return (m, c, patArgs)

-- let (x1 : A1, ..., xn : An) = e1 in e2
weakTermLetSigmaElim :: Axis -> Parser WeakTerm
weakTermLetSigmaElim axis = do
  m <- currentHint
  try $ keyword "let"
  -- xts <- parseArgList2 weakBinder
  xts <- argList $ weakBinder axis
  delimiter "="
  e1 <- weakTerm axis
  keyword "in"
  e2 <- weakTerm axis
  return $ m :< WeakTermSigmaElim xts e1 e2

weakTermLetVar :: Axis -> Parser (BinderF WeakTerm)
weakTermLetVar axis = do
  m <- currentHint
  choice
    [ try $ do
        x <- symbol
        delimiter ":"
        a <- weakTerm axis
        return (m, Ident.fromText x, a),
      do
        x <- symbol
        h <- liftIO $ Gensym.newAster (gensym axis) m
        return (m, Ident.fromText x, h)
    ]

weakTermIf :: Axis -> Parser WeakTerm
weakTermIf axis = do
  m <- currentHint
  try $ keyword "if"
  ifCond <- weakTerm axis
  keyword "then"
  ifBody <- weakTerm axis
  elseIfList <- many $ do
    keyword "else-if"
    elseIfCond <- weakTerm axis
    keyword "then"
    elseIfBody <- weakTerm axis
    return (elseIfCond, elseIfBody)
  keyword "else"
  elseBody <- weakTerm axis
  keyword "end"
  liftIO $ foldIf axis m ifCond ifBody elseIfList elseBody

foldIf :: Axis -> Hint -> WeakTerm -> WeakTerm -> [(WeakTerm, WeakTerm)] -> WeakTerm -> IO WeakTerm
foldIf axis m ifCond ifBody elseIfList elseBody =
  case elseIfList of
    [] -> do
      h <- Gensym.newAster (gensym axis) m
      return $
        m
          :< WeakTermEnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel ("bool", 1) constBoolTrue, ifBody),
              (m :< EnumCaseLabel ("bool", 0) constBoolFalse, elseBody)
            ]
    ((elseIfCond, elseIfBody) : rest) -> do
      cont <- foldIf axis m elseIfCond elseIfBody rest elseBody
      h <- Gensym.newAster (gensym axis) m
      return $
        m
          :< WeakTermEnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel ("bool", 1) constBoolTrue, ifBody),
              (m :< EnumCaseLabel ("bool", 0) constBoolFalse, cont)
            ]

weakTermParen :: Axis -> Parser WeakTerm
weakTermParen axis = do
  m <- currentHint
  es <- argList $ weakTerm axis
  case es of
    [] ->
      return $ m :< WeakTermSigmaIntro []
    [e] ->
      return e
    _ ->
      return $ m :< WeakTermSigmaIntro es

-- -- (e1, ..., en) (n >= 2)
weakTermSigmaIntro :: Axis -> Parser WeakTerm
weakTermSigmaIntro axis = do
  m <- currentHint
  try $ keyword "tuple-new"
  es <- argList $ weakTerm axis
  return $ m :< WeakTermSigmaIntro es

weakTermNoema :: Axis -> Parser WeakTerm
weakTermNoema axis = do
  m <- currentHint
  try $ delimiter "&"
  subject <- Ident.fromText <$> symbol
  t <- weakTerm axis
  return $ m :< WeakTermNoema (m :< WeakTermVar subject) t

weakTermIdealize :: Axis -> Parser WeakTerm
weakTermIdealize axis = do
  m <- currentHint
  try $ keyword "idealize"
  varList <- manyTill var (keyword "over")
  let varList' = fmap (fmap Ident.fromText) varList
  subject <- Ident.fromText <$> symbol
  e <- doBlock $ weakTerm axis
  ts <- liftIO $ mapM (\(mx, _) -> Gensym.newAster (gensym axis) mx) varList
  return $ m :< WeakTermNoemaElim subject (castLet subject (zip varList' ts) e)

castLet :: Ident -> [((Hint, Ident), WeakTerm)] -> WeakTerm -> WeakTerm
castLet subject xts cont =
  case xts of
    [] ->
      cont
    ((m, x), t) : rest ->
      bind (m, x, t) (m :< WeakTermNoemaIntro subject (weakVar' m x)) $ castLet subject rest cont

weakTermArray :: Axis -> Parser WeakTerm
weakTermArray axis = do
  m <- currentHint
  try $ keyword "array"
  betweenParen $ do
    elemType <- weakTerm axis
    return $ m :< WeakTermArray elemType

weakTermArrayIntro :: Axis -> Parser WeakTerm
weakTermArrayIntro axis = do
  m <- currentHint
  try $ keyword "array-new"
  betweenParen $ do
    elems <- sepBy (weakTerm axis) (delimiter ",")
    return $ m :< WeakTermArrayIntro (doNotCare m) elems

weakTermArrayAccess :: Axis -> Parser WeakTerm
weakTermArrayAccess axis = do
  m <- currentHint
  try $ keyword "array-access"
  betweenParen $ do
    array <- weakTerm axis
    delimiter ","
    index <- weakTerm axis
    return $ m :< WeakTermArrayAccess (doNotCare m) (doNotCare m) array index

weakTermCell :: Axis -> Parser WeakTerm
weakTermCell axis = do
  m <- currentHint
  try $ keyword "cell"
  betweenParen $ do
    contentType <- weakTerm axis
    return $ m :< WeakTermCell contentType

weakTermCellIntro :: Axis -> Parser WeakTerm
weakTermCellIntro axis = do
  m <- currentHint
  try $ keyword "cell-new"
  betweenParen $ do
    content <- weakTerm axis
    return $ m :< WeakTermCellIntro (doNotCare m) content

weakTermCellRead :: Axis -> Parser WeakTerm
weakTermCellRead axis = do
  m <- currentHint
  try $ keyword "cell-read"
  betweenParen $ do
    cell <- weakTerm axis
    return $ m :< WeakTermCellRead cell

weakTermCellWrite :: Axis -> Parser WeakTerm
weakTermCellWrite axis = do
  m <- currentHint
  try $ keyword "cell-write"
  betweenParen $ do
    cell <- weakTerm axis
    delimiter ","
    newValue <- weakTerm axis
    return $ m :< WeakTermCellWrite cell newValue

bind :: BinderF WeakTerm -> WeakTerm -> WeakTerm -> WeakTerm
bind mxt@(m, _, _) e cont =
  m :< WeakTermLet mxt e cont

weakTermAdmit :: Gensym.Axis -> Parser WeakTerm
weakTermAdmit axis = do
  m <- currentHint
  try $ keyword "admit"
  h <- liftIO $ Gensym.newAster axis m
  return $
    m
      :< WeakTermPiElim
        (weakVar m "core.os.exit")
        [ h,
          m :< WeakTermInt (m :< WeakTermConst "i64") 1
        ]

weakTermAdmitQuestion :: Gensym.Axis -> Parser WeakTerm
weakTermAdmitQuestion axis = do
  m <- currentHint
  try $ keyword "?admit"
  h <- liftIO $ Gensym.newAster axis m
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

weakTermPiElim :: Axis -> Parser WeakTerm
weakTermPiElim axis = do
  m <- currentHint
  e <- weakTermSimple axis
  impArgs <- impArgList $ weakTerm axis
  es <- argList $ weakTerm axis
  ess <- many $ argList $ weakTerm axis
  if null impArgs
    then return $ foldl' (\base args -> m :< WeakTermPiElim base args) e $ es : ess
    else do
      f <- liftIO $ Gensym.newTextualIdentFromText (axis & gensym) "func"
      h <- liftIO $ Gensym.newAster (axis & gensym) m
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

weakTermPiElimInv :: Axis -> Parser WeakTerm
weakTermPiElimInv axis = do
  m <- currentHint
  e <- weakTermSimple axis
  f <- betweenBracket $ weakTerm axis
  fs <- many $ betweenBracket $ weakTerm axis
  return $ foldl' (\base func -> m :< WeakTermPiElim func [base]) e $ f : fs

-- --
-- -- term-related helper functions
-- --

weakBinder :: Axis -> Parser (BinderF WeakTerm)
weakBinder axis =
  choice
    [ try (weakAscription axis),
      weakAscription' (axis & gensym)
    ]

weakAscription :: Axis -> Parser (BinderF WeakTerm)
weakAscription axis = do
  m <- currentHint
  x <- symbol
  delimiter ":"
  a <- weakTerm axis

  return (m, Ident.fromText x, a)

typeWithoutIdent :: Axis -> Parser (BinderF WeakTerm)
typeWithoutIdent axis = do
  m <- currentHint
  x <- liftIO $ Gensym.newTextualIdentFromText (axis & gensym) "_"
  t <- weakTerm axis
  return (m, x, t)

weakAscription' :: Gensym.Axis -> Parser (BinderF WeakTerm)
weakAscription' axis = do
  (m, x) <- weakSimpleIdent
  h <- liftIO $ Gensym.newAster axis m
  return (m, x, h)

weakSimpleIdent :: Parser (Hint, Ident)
weakSimpleIdent = do
  m <- currentHint
  x <- symbol
  return (m, Ident.fromText x)

weakTermIntrospect :: Axis -> Parser WeakTerm
weakTermIntrospect axis = do
  m <- currentHint
  try $ keyword "introspect"
  key <- symbol
  value <- liftIO $ getIntrospectiveValue axis m key
  keyword "with"
  clauseList <- many $ weakTermIntrospectiveClause axis
  keyword "end"
  case lookup value clauseList of
    Just clause ->
      return clause
    Nothing -> do
      liftIO $ putStrLn "weakTermIntrospect (not implemented)"
      undefined

-- liftIO $ outputError m $ "`" <> value <> "` is not supported here"

weakTermIntrospectiveClause :: Axis -> Parser (T.Text, WeakTerm)
weakTermIntrospectiveClause axis = do
  delimiter "-"
  c <- symbol
  delimiter "->"
  body <- weakTerm axis
  return (c, body)

getIntrospectiveValue :: Axis -> Hint -> T.Text -> IO T.Text
getIntrospectiveValue axis m key =
  case key of
    "target-platform" -> do
      T.pack <$> readIORef targetPlatformRef
    "target-arch" ->
      T.pack <$> readIORef targetArchRef
    "target-os" ->
      T.pack <$> readIORef targetOSRef
    _ ->
      (axis & throw & Throw.raiseError) m $ "no such introspective value is defined: " <> key

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

weakTermInteger :: Gensym.Axis -> Parser WeakTerm
weakTermInteger axis = do
  m <- currentHint
  intValue <- try integer
  h <- liftIO $ Gensym.newAster axis m
  return $ m :< WeakTermInt h intValue

weakTermFloat :: Gensym.Axis -> Parser WeakTerm
weakTermFloat axis = do
  m <- currentHint
  floatValue <- try float
  h <- liftIO $ Gensym.newAster axis m
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

-- newTextualIdentFromText :: T.Text -> IO Ident
-- newTextualIdentFromText txt = do
--   i <- newCount
--   newIdentFromText $ ";" <> txt <> T.pack (show i)
