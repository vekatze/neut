module Scene.Parse.PreTerm
  ( preTerm,
    preTermSimple,
    preBinder,
    preAscription,
    parseTopDefInfo,
    parseDefiniteDescription,
    preVar,
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
import qualified Entity.GlobalLocator as GL
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reflect as Ident
import Entity.LamKind
import qualified Entity.LocalLocator as LL
import Entity.LowType
import Entity.Magic
import Entity.Pattern
import qualified Entity.PreTerm as PT
import Entity.PrimNum.FromText
import qualified Entity.TargetPlatform as TP
import qualified Entity.UnresolvedName as UN
import Scene.Parse.Core
import Text.Megaparsec

--
-- parser for PT.PreTerm
--

preTerm :: Context -> Parser PT.PreTerm
preTerm ctx = do
  m <- currentHint
  e1 <- preTermEasy ctx
  choice
    [ preTermVoid ctx m e1,
      preTermExplicitAscription ctx m e1,
      return e1
    ]

-- fixme: easy??
preTermEasy :: Context -> Parser PT.PreTerm
preTermEasy ctx = do
  choice
    [ preTermPiIntro ctx,
      preTermPiIntroDef ctx,
      preTermSigma ctx,
      preTermSigmaIntro ctx,
      preTermEnumElim ctx,
      preTermIntrospect ctx,
      preTermQuestion ctx,
      preTermMagic ctx,
      preTermMatch ctx,
      preTermMatchNoetic ctx,
      preTermIf ctx,
      preTermIdealize ctx,
      preTermArray ctx,
      preTermArrayIntro ctx,
      preTermArrayAccess ctx,
      preTermText,
      preTermCell ctx,
      preTermCellIntro ctx,
      preTermCellRead ctx,
      preTermCellWrite ctx,
      preTermNoema ctx,
      try $ preTermLetSigmaElim ctx,
      preTermLet ctx,
      preTermLetCoproduct ctx,
      try $ preTermPi ctx,
      try $ preTermPiElim ctx,
      try $ preTermPiElimInv ctx,
      preTermSimple ctx
    ]

preTermSimple :: Context -> Parser PT.PreTerm
preTermSimple ctx = do
  choice
    [ preTermParen ctx,
      preTermTau,
      preTermTextIntro,
      preTermAdmitQuestion (gensym ctx),
      preTermAdmit (gensym ctx),
      preTermAster ctx,
      preTermInteger (gensym ctx),
      preTermFloat (gensym ctx),
      preTermDefiniteDescription,
      preTermVar
    ]

preTermLet :: Context -> Parser PT.PreTerm
preTermLet ctx = do
  m <- currentHint
  try $ keyword "let"
  x <- preTermLetVar ctx
  delimiter "="
  e1 <- preTerm ctx
  keyword "in"
  e2 <- preTerm ctx
  return $ m :< PT.Let x e1 e2

-- let? x : A = e1 in e2
-- let? x     = e1 in e2
preTermLetCoproduct :: Context -> Parser PT.PreTerm
preTermLetCoproduct ctx = do
  m <- currentHint
  try $ keyword "let?"
  x <- preTermLetVar ctx
  delimiter "="
  e1 <- preTerm ctx
  keyword "in"
  e2 <- preTerm ctx
  err <- liftIO $ Gensym.newTextualIdentFromText (gensym ctx) "err"
  typeOfLeft <- liftIO $ Gensym.newPreAster (gensym ctx) m
  typeOfRight <- liftIO $ Gensym.newPreAster (gensym ctx) m
  let sumLeft = Left $ UN.UnresolvedName "base.sum::left"
  let sumRight = Left $ UN.UnresolvedName "base.sum::right"
  let sumLeftVar = Ident.fromText "sum.left"
  return $
    m
      :< PT.Match
        Nothing
        (e1, doNotCare m)
        [ ( (m, sumLeft, [(m, err, typeOfLeft)]),
            m :< PT.PiElim (preVar' m sumLeftVar) [typeOfLeft, typeOfRight, preVar' m err]
          ),
          ( (m, sumRight, [x]),
            e2
          )
        ]

preTermVoid :: Context -> Hint -> PT.PreTerm -> Parser PT.PreTerm
preTermVoid ctx m e1 = do
  delimiter ";"
  e2 <- preTerm ctx
  f <- liftIO $ Gensym.newTextualIdentFromText (gensym ctx) "unit"
  return $ bind (m, f, m :< PT.Enum constTop) e1 e2

preTermExplicitAscription :: Context -> Hint -> PT.PreTerm -> Parser PT.PreTerm
preTermExplicitAscription ctx m e = do
  delimiter ":"
  t <- preTermEasy ctx
  f <- liftIO $ Gensym.newTextualIdentFromText (gensym ctx) "unit"
  return $ bind (m, f, t) e (m :< PT.Var f)

preTermTau :: Parser PT.PreTerm
preTermTau = do
  m <- currentHint
  try $ keyword "tau"
  return $ m :< PT.Tau

preTermAster :: Context -> Parser PT.PreTerm
preTermAster ctx = do
  m <- currentHint
  delimiter "?"
  liftIO $ Gensym.newPreAster (gensym ctx) m

preTermPi :: Context -> Parser PT.PreTerm
preTermPi ctx = do
  m <- currentHint
  domList <- argList $ choice [try (preAscription ctx), typeWithoutIdent ctx]
  delimiter "->"
  cod <- preTerm ctx
  return $ m :< PT.Pi domList cod

preTermPiIntro :: Context -> Parser PT.PreTerm
preTermPiIntro ctx = do
  m <- currentHint
  try $ keyword "lambda"
  varList <- argList $ preBinder ctx
  e <- preTermDotBind ctx <|> doBlock (preTerm ctx)
  return $ lam m varList e

preTermDotBind :: Context -> Parser PT.PreTerm
preTermDotBind ctx = do
  delimiter "."
  preTerm ctx

parseDefInfo :: Context -> Parser PT.DefInfo
parseDefInfo ctx = do
  functionVar <- var
  domInfoList <- argList $ preBinder ctx
  delimiter ":"
  codType <- preTerm ctx
  e <- asBlock (preTerm ctx)
  return (functionVar, domInfoList, codType, e)

parseTopDefInfo :: Context -> Parser PT.TopDefInfo
parseTopDefInfo ctx = do
  m <- currentHint
  funcBaseName <- baseName
  impDomInfoList <- impArgList $ preBinder ctx
  domInfoList <- argList $ preBinder ctx
  delimiter ":"
  codType <- preTerm ctx
  e <- asBlock (preTerm ctx)
  return ((m, funcBaseName), impDomInfoList, domInfoList, codType, e)

-- define name(x1: A1, ..., xn: An)[: A] as e end
preTermPiIntroDef :: Context -> Parser PT.PreTerm
preTermPiIntroDef ctx = do
  m <- currentHint
  try $ keyword "define"
  ((mFun, functionName), domBinderList, codType, e) <- parseDefInfo ctx
  let piType = mFun :< PT.Pi domBinderList codType
  return $ m :< PT.PiIntro (LamKindFix (mFun, Ident.fromText functionName, piType)) domBinderList e

preTermSigma :: Context -> Parser PT.PreTerm
preTermSigma ctx = do
  m <- currentHint
  try $ keyword "tuple"
  ts <- argList $ choice [try $ preAscription ctx, typeWithoutIdent ctx]
  return $ m :< PT.Sigma ts

parseDefiniteDescription :: Parser (Hint, GL.GlobalLocator, LL.LocalLocator)
parseDefiniteDescription = do
  m <- currentHint
  globalLocator <- symbol
  globalLocator' <- liftIO $ GL.reflect globalLocator
  delimiter definiteSep
  localLocator <- parseLocalLocator
  return (m, globalLocator', localLocator)

preTermDefiniteDescription :: Parser PT.PreTerm
preTermDefiniteDescription = do
  (m, globalLocator, localLocator) <- try parseDefiniteDescription
  return $ m :< PT.VarGlobal globalLocator localLocator

parseLocalLocator :: Parser LL.LocalLocator
parseLocalLocator =
  LL.reflect <$> symbol

preTermEnumElim :: Context -> Parser PT.PreTerm
preTermEnumElim ctx = do
  m <- currentHint
  try $ keyword "switch"
  e <- preTerm ctx
  keyword "with"
  clauseList <- many (preTermEnumClause ctx)
  keyword "end"
  h <- liftIO $ Gensym.newPreAster (gensym ctx) m
  return $ m :< PT.EnumElim (e, h) clauseList

preTermEnumClause :: Context -> Parser (PreEnumCase, PT.PreTerm)
preTermEnumClause ctx = do
  m <- currentHint
  delimiter "-"
  c <- symbol
  delimiter "->"
  body <- preTerm ctx
  case c of
    "default" ->
      return (m :< EnumCaseDefault, body)
    _ ->
      return (m :< EnumCaseLabel (dummyLabel c), body)

dummyLabel :: T.Text -> PreEnumLabel
dummyLabel c =
  PreEnumLabel
    (UN.UnresolvedName "base.top::unit")
    D.zero
    (UN.UnresolvedName c)

-- question e
preTermQuestion :: Context -> Parser PT.PreTerm
preTermQuestion ctx = do
  m <- currentHint
  try $ keyword "question"
  e <- preTerm ctx
  h <- liftIO $ Gensym.newPreAster (gensym ctx) m
  return $ m :< PT.Question e h

preTermMagic :: Context -> Parser PT.PreTerm
preTermMagic ctx = do
  m <- currentHint
  try $ keyword "magic"
  choice
    [ preTermMagicCast ctx m,
      preTermMagicStore ctx m,
      preTermMagicLoad ctx m,
      preTermMagicSyscall ctx m,
      preTermMagicExternal ctx m
    ]

preTermMagicBase :: T.Text -> Parser PT.PreTerm -> Parser PT.PreTerm
preTermMagicBase k parser = do
  keyword k
  betweenParen parser

preTermMagicCast :: Context -> Hint -> Parser PT.PreTerm
preTermMagicCast ctx m = do
  preTermMagicBase "cast" $ do
    castFrom <- preTerm ctx
    castTo <- delimiter "," >> preTerm ctx
    value <- delimiter "," >> preTerm ctx
    return $ m :< PT.Magic (MagicCast castFrom castTo value)

preTermMagicStore :: Context -> Hint -> Parser PT.PreTerm
preTermMagicStore ctx m = do
  preTermMagicBase "store" $ do
    lt <- lowType
    pointer <- delimiter "," >> preTerm ctx
    value <- delimiter "," >> preTerm ctx
    return $ m :< PT.Magic (MagicStore lt pointer value)

preTermMagicLoad :: Context -> Hint -> Parser PT.PreTerm
preTermMagicLoad ctx m = do
  preTermMagicBase "load" $ do
    lt <- lowType
    pointer <- delimiter "," >> preTerm ctx
    return $ m :< PT.Magic (MagicLoad lt pointer)

preTermMagicSyscall :: Context -> Hint -> Parser PT.PreTerm
preTermMagicSyscall ctx m = do
  preTermMagicBase "syscall" $ do
    syscallNum <- integer
    es <- many (delimiter "," >> preTerm ctx)
    return $ m :< PT.Magic (MagicSyscall syscallNum es)

preTermMagicExternal :: Context -> Hint -> Parser PT.PreTerm
preTermMagicExternal ctx m = do
  preTermMagicBase "external" $ do
    extFunName <- symbol
    es <- many (delimiter "," >> preTerm ctx)
    return $ m :< PT.Magic (MagicExternal extFunName es)

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

preTermMatch :: Context -> Parser PT.PreTerm
preTermMatch ctx = do
  m <- currentHint
  try $ keyword "match"
  e <- preTerm ctx
  clauseList <- withBlock $ manyList $ preTermMatchClause ctx
  return $ m :< PT.Match Nothing (e, doNotCare m) clauseList

preTermMatchNoetic :: Context -> Parser PT.PreTerm
preTermMatchNoetic ctx = do
  m <- currentHint
  try $ keyword "match-noetic"
  e <- preTerm ctx
  keyword "with"
  s <- liftIO $ Gensym.newPreAster (gensym ctx) m
  t <- liftIO $ Gensym.newPreAster (gensym ctx) m
  let e' = castFromNoema s t e
  clauseList <- manyList $ preTermMatchClause ctx
  keyword "end"
  let clauseList' = map (modifyPrePattern s) clauseList
  return $ m :< PT.Match (Just s) (e', doNotCare m) clauseList'

preTermMatchClause :: Context -> Parser (PrePatternF PT.PreTerm, PT.PreTerm)
preTermMatchClause ctx = do
  pat <- preTermPattern ctx
  delimiter "->"
  body <- preTerm ctx
  return (pat, body)

modifyPrePattern :: PT.PreTerm -> (PrePatternF PT.PreTerm, PT.PreTerm) -> (PrePatternF PT.PreTerm, PT.PreTerm)
modifyPrePattern s ((m, a, xts), body) =
  ((m, a, xts), modifyPrePatternBody s xts body)

modifyPrePatternBody :: PT.PreTerm -> [BinderF PT.PreTerm] -> PT.PreTerm -> PT.PreTerm
modifyPrePatternBody s xts body =
  case xts of
    [] ->
      body
    ((m, x, t) : rest) ->
      bind (m, x, wrapWithNoema s t) (castToNoema s t (preVar' m x)) $
        modifyPrePatternBody s rest body

preTermPattern :: Context -> Parser (PrePatternF PT.PreTerm)
preTermPattern ctx = do
  m <- currentHint
  c <- symbol
  patArgs <- argList $ preBinder ctx
  return (m, Left $ UN.UnresolvedName c, patArgs)

-- let (x1 : A1, ..., xn : An) = e1 in e2
preTermLetSigmaElim :: Context -> Parser PT.PreTerm
preTermLetSigmaElim ctx = do
  m <- currentHint
  try $ keyword "let"
  -- xts <- parseArgList2 preBinder
  xts <- argList $ preBinder ctx
  delimiter "="
  e1 <- preTerm ctx
  keyword "in"
  e2 <- preTerm ctx
  return $ m :< PT.SigmaElim xts e1 e2

preTermLetVar :: Context -> Parser (BinderF PT.PreTerm)
preTermLetVar ctx = do
  m <- currentHint
  choice
    [ try $ do
        x <- symbol
        delimiter ":"
        a <- preTerm ctx
        return (m, Ident.fromText x, a),
      do
        x <- symbol
        h <- liftIO $ Gensym.newPreAster (gensym ctx) m
        return (m, Ident.fromText x, h)
    ]

preTermIf :: Context -> Parser PT.PreTerm
preTermIf ctx = do
  m <- currentHint
  try $ keyword "if"
  ifCond <- preTerm ctx
  keyword "then"
  ifBody <- preTerm ctx
  elseIfList <- many $ do
    keyword "else-if"
    elseIfCond <- preTerm ctx
    keyword "then"
    elseIfBody <- preTerm ctx
    return (elseIfCond, elseIfBody)
  keyword "else"
  elseBody <- preTerm ctx
  keyword "end"
  liftIO $ foldIf ctx m ifCond ifBody elseIfList elseBody

foldIf :: Context -> Hint -> PT.PreTerm -> PT.PreTerm -> [(PT.PreTerm, PT.PreTerm)] -> PT.PreTerm -> IO PT.PreTerm
foldIf ctx m ifCond ifBody elseIfList elseBody =
  case elseIfList of
    [] -> do
      h <- Gensym.newPreAster (gensym ctx) m
      return $
        m
          :< PT.EnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel (weakenEnumLabel enumLabelBoolTrue), ifBody),
              (m :< EnumCaseLabel (weakenEnumLabel enumLabelBoolFalse), elseBody)
            ]
    ((elseIfCond, elseIfBody) : rest) -> do
      cont <- foldIf ctx m elseIfCond elseIfBody rest elseBody
      h <- Gensym.newPreAster (gensym ctx) m
      return $
        m
          :< PT.EnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel (weakenEnumLabel enumLabelBoolTrue), ifBody),
              (m :< EnumCaseLabel (weakenEnumLabel enumLabelBoolFalse), cont)
            ]

preTermParen :: Context -> Parser PT.PreTerm
preTermParen ctx = do
  m <- currentHint
  es <- argList $ preTerm ctx
  case es of
    [] ->
      return $ m :< PT.SigmaIntro []
    [e] ->
      return e
    _ ->
      return $ m :< PT.SigmaIntro es

-- -- (e1, ..., en) (n >= 2)
preTermSigmaIntro :: Context -> Parser PT.PreTerm
preTermSigmaIntro ctx = do
  m <- currentHint
  try $ keyword "tuple-new"
  es <- argList $ preTerm ctx
  return $ m :< PT.SigmaIntro es

preTermNoema :: Context -> Parser PT.PreTerm
preTermNoema ctx = do
  m <- currentHint
  try $ delimiter "&"
  subject <- Ident.fromText <$> symbol
  t <- preTerm ctx
  return $ m :< PT.Noema (m :< PT.Var subject) t

preTermIdealize :: Context -> Parser PT.PreTerm
preTermIdealize ctx = do
  m <- currentHint
  try $ keyword "idealize"
  varList <- manyTill var (keyword "over")
  let varList' = fmap (fmap Ident.fromText) varList
  subject <- Ident.fromText <$> symbol
  e <- doBlock $ preTerm ctx
  ts <- liftIO $ mapM (\(mx, _) -> Gensym.newPreAster (gensym ctx) mx) varList
  return $ m :< PT.NoemaElim subject (castLet subject (zip varList' ts) e)

castLet :: Ident -> [((Hint, Ident), PT.PreTerm)] -> PT.PreTerm -> PT.PreTerm
castLet subject xts cont =
  case xts of
    [] ->
      cont
    ((m, x), t) : rest ->
      bind (m, x, t) (m :< PT.NoemaIntro subject (preVar' m x)) $ castLet subject rest cont

preTermArray :: Context -> Parser PT.PreTerm
preTermArray ctx = do
  m <- currentHint
  try $ keyword "array"
  betweenParen $ do
    elemType <- preTerm ctx
    return $ m :< PT.Array elemType

preTermArrayIntro :: Context -> Parser PT.PreTerm
preTermArrayIntro ctx = do
  m <- currentHint
  try $ keyword "array-new"
  betweenParen $ do
    elems <- sepBy (preTerm ctx) (delimiter ",")
    return $ m :< PT.ArrayIntro (doNotCare m) elems

preTermArrayAccess :: Context -> Parser PT.PreTerm
preTermArrayAccess ctx = do
  m <- currentHint
  try $ keyword "array-access"
  betweenParen $ do
    array <- preTerm ctx
    delimiter ","
    index <- preTerm ctx
    return $ m :< PT.ArrayAccess (doNotCare m) (doNotCare m) array index

preTermCell :: Context -> Parser PT.PreTerm
preTermCell ctx = do
  m <- currentHint
  try $ keyword "cell"
  betweenParen $ do
    contentType <- preTerm ctx
    return $ m :< PT.Cell contentType

preTermCellIntro :: Context -> Parser PT.PreTerm
preTermCellIntro ctx = do
  m <- currentHint
  try $ keyword "cell-new"
  betweenParen $ do
    content <- preTerm ctx
    return $ m :< PT.CellIntro (doNotCare m) content

preTermCellRead :: Context -> Parser PT.PreTerm
preTermCellRead ctx = do
  m <- currentHint
  try $ keyword "cell-read"
  betweenParen $ do
    cell <- preTerm ctx
    return $ m :< PT.CellRead cell

preTermCellWrite :: Context -> Parser PT.PreTerm
preTermCellWrite ctx = do
  m <- currentHint
  try $ keyword "cell-write"
  betweenParen $ do
    cell <- preTerm ctx
    delimiter ","
    newValue <- preTerm ctx
    return $ m :< PT.CellWrite cell newValue

bind :: BinderF PT.PreTerm -> PT.PreTerm -> PT.PreTerm -> PT.PreTerm
bind mxt@(m, _, _) e cont =
  m :< PT.Let mxt e cont

preTermAdmit :: Gensym.Context -> Parser PT.PreTerm
preTermAdmit ctx = do
  m <- currentHint
  try $ keyword "admit"
  h <- liftIO $ Gensym.newPreAster ctx m
  return $
    m
      :< PT.PiElim
        (preVar m "core.os.exit")
        [ h,
          m :< PT.Int (PT.i64 m) 1
        ]

preTermAdmitQuestion :: Gensym.Context -> Parser PT.PreTerm
preTermAdmitQuestion ctx = do
  m <- currentHint
  try $ keyword "?admit"
  h <- liftIO $ Gensym.newPreAster ctx m
  return $
    m
      :< PT.Question
        ( m
            :< PT.PiElim
              (preVar m "os.exit")
              [ h,
                m :< PT.Int (PT.i64 m) 1
              ]
        )
        h

preTermPiElim :: Context -> Parser PT.PreTerm
preTermPiElim ctx = do
  m <- currentHint
  e <- preTermSimple ctx
  impArgs <- impArgList $ preTerm ctx
  es <- argList $ preTerm ctx
  ess <- many $ argList $ preTerm ctx
  if null impArgs
    then return $ foldl' (\base args -> m :< PT.PiElim base args) e $ es : ess
    else do
      f <- liftIO $ Gensym.newTextualIdentFromText (gensym ctx) "func"
      h <- liftIO $ Gensym.newPreAster (gensym ctx) m
      return $
        m
          :< PT.Let
            (m, f, h)
            e
            ( foldl'
                (\base args -> m :< PT.PiElim base args)
                (m :< PT.Var f)
                ((impArgs ++ es) : ess)
            )

preTermPiElimInv :: Context -> Parser PT.PreTerm
preTermPiElimInv ctx = do
  m <- currentHint
  e <- preTermSimple ctx
  f <- betweenBracket $ preTerm ctx
  fs <- many $ betweenBracket $ preTerm ctx
  return $ foldl' (\base func -> m :< PT.PiElim func [base]) e $ f : fs

-- --
-- -- term-related helper functions
-- --

preBinder :: Context -> Parser (BinderF PT.PreTerm)
preBinder ctx =
  choice
    [ try (preAscription ctx),
      preAscription' (gensym ctx)
    ]

preAscription :: Context -> Parser (BinderF PT.PreTerm)
preAscription ctx = do
  m <- currentHint
  x <- symbol
  delimiter ":"
  a <- preTerm ctx

  return (m, Ident.fromText x, a)

typeWithoutIdent :: Context -> Parser (BinderF PT.PreTerm)
typeWithoutIdent ctx = do
  m <- currentHint
  x <- liftIO $ Gensym.newTextualIdentFromText (gensym ctx) "_"
  t <- preTerm ctx
  return (m, x, t)

preAscription' :: Gensym.Context -> Parser (BinderF PT.PreTerm)
preAscription' ctx = do
  (m, x) <- preSimpleIdent
  h <- liftIO $ Gensym.newPreAster ctx m
  return (m, x, h)

preSimpleIdent :: Parser (Hint, Ident)
preSimpleIdent = do
  m <- currentHint
  x <- symbol
  return (m, Ident.fromText x)

preTermIntrospect :: Context -> Parser PT.PreTerm
preTermIntrospect ctx = do
  m <- currentHint
  try $ keyword "introspect"
  key <- symbol
  value <- liftIO $ getIntrospectiveValue ctx m key
  keyword "with"
  clauseList <- many $ preTermIntrospectiveClause ctx
  keyword "end"
  case lookup value clauseList of
    Just clause ->
      return clause
    Nothing -> do
      liftIO $ Throw.raiseError (throw ctx) m $ "a clause for `" <> value <> "` is missing"

preTermIntrospectiveClause :: Context -> Parser (T.Text, PT.PreTerm)
preTermIntrospectiveClause ctx = do
  delimiter "-"
  c <- symbol
  delimiter "->"
  body <- preTerm ctx
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

preTermVar :: Parser PT.PreTerm
preTermVar = do
  (m, x) <- var
  return (preVar m x)

preTermText :: Parser PT.PreTerm
preTermText = do
  m <- currentHint
  try $ keyword "text"
  return $ m :< PT.Text

preTermTextIntro :: Parser PT.PreTerm
preTermTextIntro = do
  m <- currentHint
  s <- string
  return $ m :< PT.TextIntro s

preTermInteger :: Gensym.Context -> Parser PT.PreTerm
preTermInteger ctx = do
  m <- currentHint
  intValue <- try integer
  h <- liftIO $ Gensym.newPreAster ctx m
  return $ m :< PT.Int h intValue

preTermFloat :: Gensym.Context -> Parser PT.PreTerm
preTermFloat ctx = do
  m <- currentHint
  floatValue <- try float
  h <- liftIO $ Gensym.newPreAster ctx m
  return $ m :< PT.Float h floatValue

castFromNoema :: PT.PreTerm -> PT.PreTerm -> PT.PreTerm -> PT.PreTerm
castFromNoema subject baseType tree@(m :< _) = do
  m :< PT.Magic (MagicCast (wrapWithNoema subject baseType) baseType tree)

castToNoema :: PT.PreTerm -> PT.PreTerm -> PT.PreTerm -> PT.PreTerm
castToNoema subject baseType tree@(m :< _) = do
  m :< PT.Magic (MagicCast baseType (wrapWithNoema subject baseType) tree)

wrapWithNoema :: PT.PreTerm -> PT.PreTerm -> PT.PreTerm
wrapWithNoema subject baseType@(m :< _) = do
  m :< PT.Noema subject baseType

doNotCare :: Hint -> PT.PreTerm
doNotCare m =
  m :< PT.Tau

lam :: Hint -> [BinderF PT.PreTerm] -> PT.PreTerm -> PT.PreTerm
lam m varList e =
  m :< PT.PiIntro LamKindNormal varList e

preVar :: Hint -> T.Text -> PT.PreTerm
preVar m str =
  m :< PT.Var (Ident.fromText str)

preVar' :: Hint -> Ident -> PT.PreTerm
preVar' m ident =
  m :< PT.Var ident
