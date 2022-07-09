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
import Data.List
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Binder
import Entity.Const
import qualified Entity.Discriminant as D
import Entity.EnumCase
import Entity.EnumInfo
import qualified Entity.EnumValueName as EV
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reflect as Ident
import Entity.LamKind
import Entity.LowType
import Entity.Magic
import Entity.Pattern
import Entity.PrimNum.FromText
import qualified Entity.TargetPlatform as TP
import Entity.WeakTerm
import Scene.Parse.Core
import Text.Megaparsec

--
-- parser for WeakTerm
--

weakTerm :: Context -> Parser WeakTerm
weakTerm ctx = do
  m <- currentHint
  e1 <- weakTermEasy ctx
  choice
    [ weakTermVoid ctx m e1,
      weakTermExplicitAscription ctx m e1,
      return e1
    ]

-- fixme: easy??
weakTermEasy :: Context -> Parser WeakTerm
weakTermEasy ctx = do
  choice
    [ weakTermPiIntro ctx,
      weakTermPiIntroDef ctx,
      weakTermSigma ctx,
      weakTermSigmaIntro ctx,
      weakTermEnumElim ctx,
      weakTermIntrospect ctx,
      weakTermQuestion ctx,
      weakTermMagic ctx,
      weakTermMatch ctx,
      weakTermMatchNoetic ctx,
      weakTermIf ctx,
      weakTermIdealize ctx,
      weakTermArray ctx,
      weakTermArrayIntro ctx,
      weakTermArrayAccess ctx,
      weakTermText,
      weakTermCell ctx,
      weakTermCellIntro ctx,
      weakTermCellRead ctx,
      weakTermCellWrite ctx,
      weakTermNoema ctx,
      try $ weakTermLetSigmaElim ctx,
      weakTermLet ctx,
      weakTermLetCoproduct ctx,
      try $ weakTermPi ctx,
      try $ weakTermPiElim ctx,
      try $ weakTermPiElimInv ctx,
      weakTermSimple ctx
    ]

weakTermSimple :: Context -> Parser WeakTerm
weakTermSimple ctx = do
  choice
    [ weakTermParen ctx,
      weakTermTau,
      weakTermTextIntro,
      weakTermAdmitQuestion (gensym ctx),
      weakTermAdmit (gensym ctx),
      weakTermAster ctx,
      weakTermInteger (gensym ctx),
      weakTermFloat (gensym ctx),
      weakTermDefiniteDescription,
      weakTermVar
    ]

weakTermLet :: Context -> Parser WeakTerm
weakTermLet ctx = do
  m <- currentHint
  try $ keyword "let"
  x <- weakTermLetVar ctx
  delimiter "="
  e1 <- weakTerm ctx
  keyword "in"
  e2 <- weakTerm ctx
  return $ m :< WeakTermLet x e1 e2

-- let? x : A = e1 in e2
-- let? x     = e1 in e2
weakTermLetCoproduct :: Context -> Parser WeakTerm
weakTermLetCoproduct ctx = do
  m <- currentHint
  try $ keyword "let?"
  x <- weakTermLetVar ctx
  delimiter "="
  e1 <- weakTerm ctx
  keyword "in"
  e2 <- weakTerm ctx
  err <- liftIO $ Gensym.newTextualIdentFromText (gensym ctx) "err"
  typeOfLeft <- liftIO $ Gensym.newAster (gensym ctx) m
  typeOfRight <- liftIO $ Gensym.newAster (gensym ctx) m
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

weakTermVoid :: Context -> Hint -> WeakTerm -> Parser WeakTerm
weakTermVoid ctx m e1 = do
  delimiter ";"
  e2 <- weakTerm ctx
  f <- liftIO $ Gensym.newTextualIdentFromText (gensym ctx) "unit"
  return $ bind (m, f, m :< WeakTermEnum constTop) e1 e2

weakTermExplicitAscription :: Context -> Hint -> WeakTerm -> Parser WeakTerm
weakTermExplicitAscription ctx m e = do
  delimiter ":"
  t <- weakTermEasy ctx
  f <- liftIO $ Gensym.newTextualIdentFromText (gensym ctx) "unit"
  return $ bind (m, f, t) e (m :< WeakTermVar f)

weakTermTau :: Parser WeakTerm
weakTermTau = do
  m <- currentHint
  try $ keyword "tau"
  return $ m :< WeakTermTau

weakTermAster :: Context -> Parser WeakTerm
weakTermAster ctx = do
  m <- currentHint
  delimiter "?"
  liftIO $ Gensym.newAster (gensym ctx) m

weakTermPi :: Context -> Parser WeakTerm
weakTermPi ctx = do
  m <- currentHint
  domList <- argList $ choice [try (weakAscription ctx), typeWithoutIdent ctx]
  delimiter "->"
  cod <- weakTerm ctx
  return $ m :< WeakTermPi domList cod

weakTermPiIntro :: Context -> Parser WeakTerm
weakTermPiIntro ctx = do
  m <- currentHint
  try $ keyword "lambda"
  varList <- argList $ weakBinder ctx
  e <- weakTermDotBind ctx <|> doBlock (weakTerm ctx)
  return $ lam m varList e

weakTermDotBind :: Context -> Parser WeakTerm
weakTermDotBind ctx = do
  delimiter "."
  weakTerm ctx

parseDefInfo :: Context -> Parser DefInfo
parseDefInfo ctx = do
  functionVar <- var
  domInfoList <- argList $ weakBinder ctx
  delimiter ":"
  codType <- weakTerm ctx
  e <- asBlock (weakTerm ctx)
  return (functionVar, domInfoList, codType, e)

parseTopDefInfo :: Context -> Parser TopDefInfo
parseTopDefInfo ctx = do
  functionVar <- var
  impDomInfoList <- impArgList $ weakBinder ctx
  domInfoList <- argList $ weakBinder ctx
  delimiter ":"
  codType <- weakTerm ctx
  e <- asBlock (weakTerm ctx)
  return (functionVar, impDomInfoList, domInfoList, codType, e)

-- define name(x1: A1, ..., xn: An)[: A] as e end
weakTermPiIntroDef :: Context -> Parser WeakTerm
weakTermPiIntroDef ctx = do
  m <- currentHint
  try $ keyword "define"
  ((mFun, functionName), domBinderList, codType, e) <- parseDefInfo ctx
  let piType = mFun :< WeakTermPi domBinderList codType
  return $ m :< WeakTermPiIntro (LamKindFix (mFun, Ident.fromText functionName, piType)) domBinderList e

weakTermSigma :: Context -> Parser WeakTerm
weakTermSigma ctx = do
  m <- currentHint
  try $ keyword "tuple"
  ts <- argList $ choice [try $ weakAscription ctx, typeWithoutIdent ctx]
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

weakTermEnumElim :: Context -> Parser WeakTerm
weakTermEnumElim ctx = do
  m <- currentHint
  try $ keyword "switch"
  e <- weakTerm ctx
  keyword "with"
  clauseList <- many (weakTermEnumClause ctx)
  keyword "end"
  h <- liftIO $ Gensym.newAster (gensym ctx) m
  return $ m :< WeakTermEnumElim (e, h) clauseList

weakTermEnumClause :: Context -> Parser (EnumCase, WeakTerm)
weakTermEnumClause ctx = do
  m <- currentHint
  delimiter "-"
  c <- symbol
  delimiter "->"
  body <- weakTerm ctx
  let dummyLabelInfo = (constTop, D.zero)
  case c of
    "default" ->
      return (m :< EnumCaseDefault, body)
    _ ->
      return (m :< EnumCaseLabel dummyLabelInfo (EV.EnumValueName c), body)

-- question e
weakTermQuestion :: Context -> Parser WeakTerm
weakTermQuestion ctx = do
  m <- currentHint
  try $ keyword "question"
  e <- weakTerm ctx
  h <- liftIO $ Gensym.newAster (gensym ctx) m
  return $ m :< WeakTermQuestion e h

weakTermMagic :: Context -> Parser WeakTerm
weakTermMagic ctx = do
  m <- currentHint
  try $ keyword "magic"
  choice
    [ weakTermMagicCast ctx m,
      weakTermMagicStore ctx m,
      weakTermMagicLoad ctx m,
      weakTermMagicSyscall ctx m,
      weakTermMagicExternal ctx m
    ]

weakTermMagicBase :: T.Text -> Parser WeakTerm -> Parser WeakTerm
weakTermMagicBase k parser = do
  keyword k
  betweenParen parser

weakTermMagicCast :: Context -> Hint -> Parser WeakTerm
weakTermMagicCast ctx m = do
  weakTermMagicBase "cast" $ do
    castFrom <- weakTerm ctx
    castTo <- delimiter "," >> weakTerm ctx
    value <- delimiter "," >> weakTerm ctx
    return $ m :< WeakTermMagic (MagicCast castFrom castTo value)

weakTermMagicStore :: Context -> Hint -> Parser WeakTerm
weakTermMagicStore ctx m = do
  weakTermMagicBase "store" $ do
    lt <- lowType
    pointer <- delimiter "," >> weakTerm ctx
    value <- delimiter "," >> weakTerm ctx
    return $ m :< WeakTermMagic (MagicStore lt pointer value)

weakTermMagicLoad :: Context -> Hint -> Parser WeakTerm
weakTermMagicLoad ctx m = do
  weakTermMagicBase "load" $ do
    lt <- lowType
    pointer <- delimiter "," >> weakTerm ctx
    return $ m :< WeakTermMagic (MagicLoad lt pointer)

weakTermMagicSyscall :: Context -> Hint -> Parser WeakTerm
weakTermMagicSyscall ctx m = do
  weakTermMagicBase "syscall" $ do
    syscallNum <- integer
    es <- many (delimiter "," >> weakTerm ctx)
    return $ m :< WeakTermMagic (MagicSyscall syscallNum es)

weakTermMagicExternal :: Context -> Hint -> Parser WeakTerm
weakTermMagicExternal ctx m = do
  weakTermMagicBase "external" $ do
    extFunName <- symbol
    es <- many (delimiter "," >> weakTerm ctx)
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

weakTermMatch :: Context -> Parser WeakTerm
weakTermMatch ctx = do
  m <- currentHint
  try $ keyword "match"
  e <- weakTerm ctx
  clauseList <- withBlock $ manyList $ weakTermMatchClause ctx
  return $ m :< WeakTermMatch Nothing (e, doNotCare m) clauseList

weakTermMatchNoetic :: Context -> Parser WeakTerm
weakTermMatchNoetic ctx = do
  m <- currentHint
  try $ keyword "match-noetic"
  e <- weakTerm ctx
  keyword "with"
  s <- liftIO $ Gensym.newAster (gensym ctx) m
  t <- liftIO $ Gensym.newAster (gensym ctx) m
  let e' = castFromNoema s t e
  clauseList <- manyList $ weakTermMatchClause ctx
  keyword "end"
  let clauseList' = map (modifyWeakPattern s) clauseList
  return $ m :< WeakTermMatch (Just s) (e', doNotCare m) clauseList'

weakTermMatchClause :: Context -> Parser (PatternF WeakTerm, WeakTerm)
weakTermMatchClause ctx = do
  pat <- weakTermPattern ctx
  delimiter "->"
  body <- weakTerm ctx
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

weakTermPattern :: Context -> Parser (PatternF WeakTerm)
weakTermPattern ctx = do
  m <- currentHint
  c <- symbol
  patArgs <- argList $ weakBinder ctx
  return (m, c, patArgs)

-- let (x1 : A1, ..., xn : An) = e1 in e2
weakTermLetSigmaElim :: Context -> Parser WeakTerm
weakTermLetSigmaElim ctx = do
  m <- currentHint
  try $ keyword "let"
  -- xts <- parseArgList2 weakBinder
  xts <- argList $ weakBinder ctx
  delimiter "="
  e1 <- weakTerm ctx
  keyword "in"
  e2 <- weakTerm ctx
  return $ m :< WeakTermSigmaElim xts e1 e2

weakTermLetVar :: Context -> Parser (BinderF WeakTerm)
weakTermLetVar ctx = do
  m <- currentHint
  choice
    [ try $ do
        x <- symbol
        delimiter ":"
        a <- weakTerm ctx
        return (m, Ident.fromText x, a),
      do
        x <- symbol
        h <- liftIO $ Gensym.newAster (gensym ctx) m
        return (m, Ident.fromText x, h)
    ]

weakTermIf :: Context -> Parser WeakTerm
weakTermIf ctx = do
  m <- currentHint
  try $ keyword "if"
  ifCond <- weakTerm ctx
  keyword "then"
  ifBody <- weakTerm ctx
  elseIfList <- many $ do
    keyword "else-if"
    elseIfCond <- weakTerm ctx
    keyword "then"
    elseIfBody <- weakTerm ctx
    return (elseIfCond, elseIfBody)
  keyword "else"
  elseBody <- weakTerm ctx
  keyword "end"
  liftIO $ foldIf ctx m ifCond ifBody elseIfList elseBody

foldIf :: Context -> Hint -> WeakTerm -> WeakTerm -> [(WeakTerm, WeakTerm)] -> WeakTerm -> IO WeakTerm
foldIf ctx m ifCond ifBody elseIfList elseBody =
  case elseIfList of
    [] -> do
      h <- Gensym.newAster (gensym ctx) m
      return $
        m
          :< WeakTermEnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel (constBool, D.increment D.zero) constBoolTrue, ifBody),
              (m :< EnumCaseLabel (constBool, D.zero) constBoolFalse, elseBody)
            ]
    ((elseIfCond, elseIfBody) : rest) -> do
      cont <- foldIf ctx m elseIfCond elseIfBody rest elseBody
      h <- Gensym.newAster (gensym ctx) m
      return $
        m
          :< WeakTermEnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel (constBool, D.increment D.zero) constBoolTrue, ifBody),
              (m :< EnumCaseLabel (constBool, D.zero) constBoolFalse, cont)
            ]

weakTermParen :: Context -> Parser WeakTerm
weakTermParen ctx = do
  m <- currentHint
  es <- argList $ weakTerm ctx
  case es of
    [] ->
      return $ m :< WeakTermSigmaIntro []
    [e] ->
      return e
    _ ->
      return $ m :< WeakTermSigmaIntro es

-- -- (e1, ..., en) (n >= 2)
weakTermSigmaIntro :: Context -> Parser WeakTerm
weakTermSigmaIntro ctx = do
  m <- currentHint
  try $ keyword "tuple-new"
  es <- argList $ weakTerm ctx
  return $ m :< WeakTermSigmaIntro es

weakTermNoema :: Context -> Parser WeakTerm
weakTermNoema ctx = do
  m <- currentHint
  try $ delimiter "&"
  subject <- Ident.fromText <$> symbol
  t <- weakTerm ctx
  return $ m :< WeakTermNoema (m :< WeakTermVar subject) t

weakTermIdealize :: Context -> Parser WeakTerm
weakTermIdealize ctx = do
  m <- currentHint
  try $ keyword "idealize"
  varList <- manyTill var (keyword "over")
  let varList' = fmap (fmap Ident.fromText) varList
  subject <- Ident.fromText <$> symbol
  e <- doBlock $ weakTerm ctx
  ts <- liftIO $ mapM (\(mx, _) -> Gensym.newAster (gensym ctx) mx) varList
  return $ m :< WeakTermNoemaElim subject (castLet subject (zip varList' ts) e)

castLet :: Ident -> [((Hint, Ident), WeakTerm)] -> WeakTerm -> WeakTerm
castLet subject xts cont =
  case xts of
    [] ->
      cont
    ((m, x), t) : rest ->
      bind (m, x, t) (m :< WeakTermNoemaIntro subject (weakVar' m x)) $ castLet subject rest cont

weakTermArray :: Context -> Parser WeakTerm
weakTermArray ctx = do
  m <- currentHint
  try $ keyword "array"
  betweenParen $ do
    elemType <- weakTerm ctx
    return $ m :< WeakTermArray elemType

weakTermArrayIntro :: Context -> Parser WeakTerm
weakTermArrayIntro ctx = do
  m <- currentHint
  try $ keyword "array-new"
  betweenParen $ do
    elems <- sepBy (weakTerm ctx) (delimiter ",")
    return $ m :< WeakTermArrayIntro (doNotCare m) elems

weakTermArrayAccess :: Context -> Parser WeakTerm
weakTermArrayAccess ctx = do
  m <- currentHint
  try $ keyword "array-access"
  betweenParen $ do
    array <- weakTerm ctx
    delimiter ","
    index <- weakTerm ctx
    return $ m :< WeakTermArrayAccess (doNotCare m) (doNotCare m) array index

weakTermCell :: Context -> Parser WeakTerm
weakTermCell ctx = do
  m <- currentHint
  try $ keyword "cell"
  betweenParen $ do
    contentType <- weakTerm ctx
    return $ m :< WeakTermCell contentType

weakTermCellIntro :: Context -> Parser WeakTerm
weakTermCellIntro ctx = do
  m <- currentHint
  try $ keyword "cell-new"
  betweenParen $ do
    content <- weakTerm ctx
    return $ m :< WeakTermCellIntro (doNotCare m) content

weakTermCellRead :: Context -> Parser WeakTerm
weakTermCellRead ctx = do
  m <- currentHint
  try $ keyword "cell-read"
  betweenParen $ do
    cell <- weakTerm ctx
    return $ m :< WeakTermCellRead cell

weakTermCellWrite :: Context -> Parser WeakTerm
weakTermCellWrite ctx = do
  m <- currentHint
  try $ keyword "cell-write"
  betweenParen $ do
    cell <- weakTerm ctx
    delimiter ","
    newValue <- weakTerm ctx
    return $ m :< WeakTermCellWrite cell newValue

bind :: BinderF WeakTerm -> WeakTerm -> WeakTerm -> WeakTerm
bind mxt@(m, _, _) e cont =
  m :< WeakTermLet mxt e cont

weakTermAdmit :: Gensym.Context -> Parser WeakTerm
weakTermAdmit ctx = do
  m <- currentHint
  try $ keyword "admit"
  h <- liftIO $ Gensym.newAster ctx m
  return $
    m
      :< WeakTermPiElim
        (weakVar m "core.os.exit")
        [ h,
          m :< WeakTermInt (m :< WeakTermConst "i64") 1
        ]

weakTermAdmitQuestion :: Gensym.Context -> Parser WeakTerm
weakTermAdmitQuestion ctx = do
  m <- currentHint
  try $ keyword "?admit"
  h <- liftIO $ Gensym.newAster ctx m
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

weakTermPiElim :: Context -> Parser WeakTerm
weakTermPiElim ctx = do
  m <- currentHint
  e <- weakTermSimple ctx
  impArgs <- impArgList $ weakTerm ctx
  es <- argList $ weakTerm ctx
  ess <- many $ argList $ weakTerm ctx
  if null impArgs
    then return $ foldl' (\base args -> m :< WeakTermPiElim base args) e $ es : ess
    else do
      f <- liftIO $ Gensym.newTextualIdentFromText (gensym ctx) "func"
      h <- liftIO $ Gensym.newAster (gensym ctx) m
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

weakTermPiElimInv :: Context -> Parser WeakTerm
weakTermPiElimInv ctx = do
  m <- currentHint
  e <- weakTermSimple ctx
  f <- betweenBracket $ weakTerm ctx
  fs <- many $ betweenBracket $ weakTerm ctx
  return $ foldl' (\base func -> m :< WeakTermPiElim func [base]) e $ f : fs

-- --
-- -- term-related helper functions
-- --

weakBinder :: Context -> Parser (BinderF WeakTerm)
weakBinder ctx =
  choice
    [ try (weakAscription ctx),
      weakAscription' (gensym ctx)
    ]

weakAscription :: Context -> Parser (BinderF WeakTerm)
weakAscription ctx = do
  m <- currentHint
  x <- symbol
  delimiter ":"
  a <- weakTerm ctx

  return (m, Ident.fromText x, a)

typeWithoutIdent :: Context -> Parser (BinderF WeakTerm)
typeWithoutIdent ctx = do
  m <- currentHint
  x <- liftIO $ Gensym.newTextualIdentFromText (gensym ctx) "_"
  t <- weakTerm ctx
  return (m, x, t)

weakAscription' :: Gensym.Context -> Parser (BinderF WeakTerm)
weakAscription' ctx = do
  (m, x) <- weakSimpleIdent
  h <- liftIO $ Gensym.newAster ctx m
  return (m, x, h)

weakSimpleIdent :: Parser (Hint, Ident)
weakSimpleIdent = do
  m <- currentHint
  x <- symbol
  return (m, Ident.fromText x)

weakTermIntrospect :: Context -> Parser WeakTerm
weakTermIntrospect ctx = do
  m <- currentHint
  try $ keyword "introspect"
  key <- symbol
  value <- liftIO $ getIntrospectiveValue ctx m key
  keyword "with"
  clauseList <- many $ weakTermIntrospectiveClause ctx
  keyword "end"
  case lookup value clauseList of
    Just clause ->
      return clause
    Nothing -> do
      liftIO $ putStrLn "weakTermIntrospect (not implemented)"
      undefined

-- liftIO $ outputError m $ "`" <> value <> "` is not supported here"

weakTermIntrospectiveClause :: Context -> Parser (T.Text, WeakTerm)
weakTermIntrospectiveClause ctx = do
  delimiter "-"
  c <- symbol
  delimiter "->"
  body <- weakTerm ctx
  return (c, body)

getIntrospectiveValue :: Context -> Hint -> T.Text -> IO T.Text
getIntrospectiveValue ctx m key =
  case key of
    "target-platform" -> do
      return $ T.pack (TP.platform (targetPlatform ctx))
    "target-arch" ->
      return $ T.pack (TP.arch (targetPlatform ctx))
    "target-os" ->
      return $ T.pack (TP.os (targetPlatform ctx))
    _ ->
      Throw.raiseError (throw ctx) m $ "no such introspective value is defined: " <> key

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

weakTermInteger :: Gensym.Context -> Parser WeakTerm
weakTermInteger ctx = do
  m <- currentHint
  intValue <- try integer
  h <- liftIO $ Gensym.newAster ctx m
  return $ m :< WeakTermInt h intValue

weakTermFloat :: Gensym.Context -> Parser WeakTerm
weakTermFloat ctx = do
  m <- currentHint
  floatValue <- try float
  h <- liftIO $ Gensym.newAster ctx m
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
