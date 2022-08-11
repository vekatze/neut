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
import Entity.EnumCase
import Entity.EnumInfo
import qualified Entity.ExternalName as EN
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

preTerm :: Context m => Parser m PT.PreTerm
preTerm = do
  m <- lift getCurrentHint
  e1 <- preTermEasy
  choice
    [ preTermVoid m e1,
      preTermExplicitAscription m e1,
      return e1
    ]

-- fixme: easy??
preTermEasy :: Context m => Parser m PT.PreTerm
preTermEasy = do
  choice
    [ preTermPiIntro,
      preTermPiIntroDef,
      preTermSigma,
      preTermSigmaIntro,
      preTermEnumElim,
      preTermIntrospect,
      preTermQuestion,
      preTermMagic,
      preTermMatch,
      preTermMatchNoetic,
      preTermIf,
      preTermIdealize,
      preTermArray,
      preTermArrayIntro,
      preTermArrayAccess,
      preTermText,
      preTermCell,
      preTermCellIntro,
      preTermCellRead,
      preTermCellWrite,
      preTermNoema,
      try preTermLetSigmaElim,
      preTermLet,
      preTermLetCoproduct,
      try preTermPi,
      try preTermPiElim,
      try preTermPiElimInv,
      preTermSimple
    ]

preTermSimple :: Context m => Parser m PT.PreTerm
preTermSimple = do
  choice
    [ preTermParen,
      preTermTau,
      preTermTextIntro,
      preTermAdmitQuestion,
      preTermAdmit,
      preTermAster,
      preTermInteger,
      preTermFloat,
      preTermDefiniteDescription,
      preTermVar
    ]

preTermLet :: Context m => Parser m PT.PreTerm
preTermLet = do
  m <- lift getCurrentHint
  try $ keyword "let"
  x <- preTermLetVar
  delimiter "="
  e1 <- preTerm
  keyword "in"
  e2 <- preTerm
  return $ m :< PT.Let x e1 e2

-- let? x : A = e1 in e2
-- let? x     = e1 in e2
preTermLetCoproduct :: Context m => Parser m PT.PreTerm
preTermLetCoproduct = do
  m <- lift getCurrentHint
  try $ keyword "let?"
  x <- preTermLetVar
  delimiter "="
  e1 <- preTerm
  keyword "in"
  e2 <- preTerm
  err <- lift $ Gensym.newTextualIdentFromText "err"
  typeOfLeft <- lift $ Gensym.newPreAster m
  typeOfRight <- lift $ Gensym.newPreAster m
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

preTermVoid :: Context m => Hint -> PT.PreTerm -> Parser m PT.PreTerm
preTermVoid m e1 = do
  delimiter ";"
  e2 <- preTerm
  f <- lift $ Gensym.newTextualIdentFromText "unit"
  return $ bind (m, f, m :< PT.Enum constTop) e1 e2

preTermExplicitAscription :: Context m => Hint -> PT.PreTerm -> Parser m PT.PreTerm
preTermExplicitAscription m e = do
  delimiter ":"
  t <- preTermEasy
  f <- lift $ Gensym.newTextualIdentFromText "unit"
  return $ bind (m, f, t) e (m :< PT.Var f)

preTermTau :: Context m => Parser m PT.PreTerm
preTermTau = do
  m <- lift getCurrentHint
  try $ keyword "tau"
  return $ m :< PT.Tau

preTermAster :: Context m => Parser m PT.PreTerm
preTermAster = do
  m <- lift getCurrentHint
  delimiter "?"
  lift $ Gensym.newPreAster m

preTermPi :: Context m => Parser m PT.PreTerm
preTermPi = do
  m <- lift getCurrentHint
  domList <- argList $ choice [try preAscription, typeWithoutIdent]
  delimiter "->"
  cod <- preTerm
  return $ m :< PT.Pi domList cod

preTermPiIntro :: Context m => Parser m PT.PreTerm
preTermPiIntro = do
  m <- lift getCurrentHint
  try $ keyword "lambda"
  varList <- argList preBinder
  e <- choice [preTermDotBind, doBlock preTerm]
  return $ lam m varList e

preTermDotBind :: Context m => Parser m PT.PreTerm
preTermDotBind = do
  delimiter "."
  preTerm

parseDefInfo :: Context m => Parser m PT.DefInfo
parseDefInfo = do
  functionVar <- var
  domInfoList <- argList preBinder
  delimiter ":"
  codType <- preTerm
  e <- asBlock preTerm
  return (functionVar, domInfoList, codType, e)

parseTopDefInfo :: Context m => Parser m PT.TopDefInfo
parseTopDefInfo = do
  m <- lift getCurrentHint
  funcBaseName <- baseName
  impDomInfoList <- impArgList preBinder
  domInfoList <- argList preBinder
  delimiter ":"
  codType <- preTerm
  e <- asBlock preTerm
  return ((m, funcBaseName), impDomInfoList, domInfoList, codType, e)

-- define name(x1: A1, ..., xn: An)[: A] as e end
preTermPiIntroDef :: Context m => Parser m PT.PreTerm
preTermPiIntroDef = do
  m <- lift getCurrentHint
  try $ keyword "define"
  ((mFun, functionName), domBinderList, codType, e) <- parseDefInfo
  let piType = mFun :< PT.Pi domBinderList codType
  return $ m :< PT.PiIntro (LamKindFix (mFun, Ident.fromText functionName, piType)) domBinderList e

preTermSigma :: Context m => Parser m PT.PreTerm
preTermSigma = do
  m <- lift getCurrentHint
  try $ keyword "tuple"
  ts <- argList $ choice [try preAscription, typeWithoutIdent]
  return $ m :< PT.Sigma ts

parseDefiniteDescription :: Context m => Parser m (Hint, GL.GlobalLocator, LL.LocalLocator)
parseDefiniteDescription = do
  m <- lift getCurrentHint
  globalLocator <- symbol
  globalLocator' <- lift $ GL.reflect m globalLocator
  delimiter definiteSep
  localLocator <- parseLocalLocator
  return (m, globalLocator', localLocator)

preTermDefiniteDescription :: Context m => Parser m PT.PreTerm
preTermDefiniteDescription = do
  (m, globalLocator, localLocator) <- try parseDefiniteDescription
  return $ m :< PT.VarGlobal globalLocator localLocator

parseLocalLocator :: Context m => Parser m LL.LocalLocator
parseLocalLocator = do
  m <- lift getCurrentHint
  rawTxt <- symbol
  lift $ LL.reflect m rawTxt

preTermEnumElim :: Context m => Parser m PT.PreTerm
preTermEnumElim = do
  m <- lift getCurrentHint
  try $ keyword "switch"
  e <- preTerm
  keyword "with"
  clauseList <- many preTermEnumClause
  keyword "end"
  h <- lift $ Gensym.newPreAster m
  return $ m :< PT.EnumElim (e, h) clauseList

preTermEnumClause :: Context m => Parser m (PreEnumCase, PT.PreTerm)
preTermEnumClause = do
  m <- lift getCurrentHint
  delimiter "-"
  c <- symbol
  delimiter "->"
  body <- preTerm
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
preTermQuestion :: Context m => Parser m PT.PreTerm
preTermQuestion = do
  m <- lift getCurrentHint
  try $ keyword "question"
  e <- preTerm
  h <- lift $ Gensym.newPreAster m
  return $ m :< PT.Question e h

preTermMagic :: Context m => Parser m PT.PreTerm
preTermMagic = do
  m <- lift getCurrentHint
  try $ keyword "magic"
  choice
    [ preTermMagicCast m,
      preTermMagicStore m,
      preTermMagicLoad m,
      preTermMagicSyscall m,
      preTermMagicExternal m
    ]

preTermMagicBase :: Context m => T.Text -> Parser m PT.PreTerm -> Parser m PT.PreTerm
preTermMagicBase k parser = do
  keyword k
  betweenParen parser

preTermMagicCast :: Context m => Hint -> Parser m PT.PreTerm
preTermMagicCast m = do
  preTermMagicBase "cast" $ do
    castFrom <- preTerm
    castTo <- delimiter "," >> preTerm
    value <- delimiter "," >> preTerm
    return $ m :< PT.Magic (MagicCast castFrom castTo value)

preTermMagicStore :: Context m => Hint -> Parser m PT.PreTerm
preTermMagicStore m = do
  preTermMagicBase "store" $ do
    lt <- lowType
    pointer <- delimiter "," >> preTerm
    value <- delimiter "," >> preTerm
    return $ m :< PT.Magic (MagicStore lt pointer value)

preTermMagicLoad :: Context m => Hint -> Parser m PT.PreTerm
preTermMagicLoad m = do
  preTermMagicBase "load" $ do
    lt <- lowType
    pointer <- delimiter "," >> preTerm
    return $ m :< PT.Magic (MagicLoad lt pointer)

preTermMagicSyscall :: Context m => Hint -> Parser m PT.PreTerm
preTermMagicSyscall m = do
  preTermMagicBase "syscall" $ do
    syscallNum <- integer
    es <- many (delimiter "," >> preTerm)
    return $ m :< PT.Magic (MagicSyscall syscallNum es)

preTermMagicExternal :: Context m => Hint -> Parser m PT.PreTerm
preTermMagicExternal m = do
  preTermMagicBase "external" $ do
    extFunName <- symbol
    es <- many (delimiter "," >> preTerm)
    return $ m :< PT.Magic (MagicExternal (EN.ExternalName extFunName) es)

-- -- t ::= i{n} | f{n} | pointer t | array INT t | struct t ... t
lowType :: Context m => Parser m LowType
lowType = do
  choice
    [ lowTypePointer,
      lowTypeArray,
      lowTypeStruct,
      lowTypeSimple
    ]

lowTypeSimple :: Context m => Parser m LowType
lowTypeSimple =
  choice
    [ betweenParen lowType,
      lowTypeNumber
    ]

lowTypePointer :: Context m => Parser m LowType
lowTypePointer = do
  keyword "pointer"
  lowTypeSimple

lowTypeArray :: Context m => Parser m LowType
lowTypeArray = do
  keyword "array"
  intValue <- integer
  LowTypeArray (fromInteger intValue) <$> lowTypeSimple

lowTypeStruct :: Context m => Parser m LowType
lowTypeStruct = do
  keyword "struct"
  LowTypeStruct <$> many lowTypeSimple

lowTypeNumber :: Context m => Parser m LowType
lowTypeNumber = do
  sizeString <- symbol
  case fromText sizeString of
    Just primNum ->
      return $ LowTypePrimNum primNum
    _ -> do
      failure (Just (asTokens sizeString)) (S.fromList [asLabel "i{n}", asLabel "f{n}"])

-- sizeString' <- asTokens sizeString
-- labelInteger <- asLabel "i{n}"
-- labelFloat <- asLabel "f{n}"
-- failure (Just sizeString') [labelInteger, labelFloat]

preTermMatch :: Context m => Parser m PT.PreTerm
preTermMatch = do
  m <- lift getCurrentHint
  try $ keyword "match"
  e <- preTerm
  clauseList <- withBlock $ manyList preTermMatchClause
  return $ m :< PT.Match Nothing (e, doNotCare m) clauseList

preTermMatchNoetic :: Context m => Parser m PT.PreTerm
preTermMatchNoetic = do
  m <- lift getCurrentHint
  try $ keyword "match-noetic"
  e <- preTerm
  keyword "with"
  s <- lift $ Gensym.newPreAster m
  t <- lift $ Gensym.newPreAster m
  let e' = castFromNoema s t e
  clauseList <- manyList preTermMatchClause
  keyword "end"
  let clauseList' = map (modifyPrePattern s) clauseList
  return $ m :< PT.Match (Just s) (e', doNotCare m) clauseList'

preTermMatchClause :: Context m => Parser m (PrePatternF PT.PreTerm, PT.PreTerm)
preTermMatchClause = do
  pat <- preTermPattern
  delimiter "->"
  body <- preTerm
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

preTermPattern :: Context m => Parser m (PrePatternF PT.PreTerm)
preTermPattern = do
  m <- lift getCurrentHint
  c <- symbol
  patArgs <- argList preBinder
  return (m, Left $ UN.UnresolvedName c, patArgs)

-- let (x1 : A1, ..., xn : An) = e1 in e2
preTermLetSigmaElim :: Context m => Parser m PT.PreTerm
preTermLetSigmaElim = do
  m <- lift getCurrentHint
  try $ keyword "let"
  -- xts <- parseArgList2 preBinder
  xts <- argList preBinder
  delimiter "="
  e1 <- preTerm
  keyword "in"
  e2 <- preTerm
  return $ m :< PT.SigmaElim xts e1 e2

preTermLetVar :: Context m => Parser m (BinderF PT.PreTerm)
preTermLetVar = do
  m <- lift getCurrentHint
  choice
    [ try $ do
        x <- symbol
        delimiter ":"
        a <- preTerm
        return (m, Ident.fromText x, a),
      do
        x <- symbol
        h <- lift $ Gensym.newPreAster m
        return (m, Ident.fromText x, h)
    ]

preTermIf :: Context m => Parser m PT.PreTerm
preTermIf = do
  m <- lift getCurrentHint
  try $ keyword "if"
  ifCond <- preTerm
  keyword "then"
  ifBody <- preTerm
  elseIfList <- many $ do
    keyword "else-if"
    elseIfCond <- preTerm
    keyword "then"
    elseIfBody <- preTerm
    return (elseIfCond, elseIfBody)
  keyword "else"
  elseBody <- preTerm
  keyword "end"
  foldIf m ifCond ifBody elseIfList elseBody

foldIf :: Context m => Hint -> PT.PreTerm -> PT.PreTerm -> [(PT.PreTerm, PT.PreTerm)] -> PT.PreTerm -> Parser m PT.PreTerm
foldIf m ifCond ifBody elseIfList elseBody =
  case elseIfList of
    [] -> do
      h <- lift $ Gensym.newPreAster m
      return $
        m
          :< PT.EnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel (weakenEnumLabel enumLabelBoolTrue), ifBody),
              (m :< EnumCaseLabel (weakenEnumLabel enumLabelBoolFalse), elseBody)
            ]
    ((elseIfCond, elseIfBody) : rest) -> do
      cont <- foldIf m elseIfCond elseIfBody rest elseBody
      h <- lift $ Gensym.newPreAster m
      return $
        m
          :< PT.EnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel (weakenEnumLabel enumLabelBoolTrue), ifBody),
              (m :< EnumCaseLabel (weakenEnumLabel enumLabelBoolFalse), cont)
            ]

preTermParen :: Context m => Parser m PT.PreTerm
preTermParen = do
  m <- lift getCurrentHint
  es <- argList preTerm
  case es of
    [] ->
      return $ m :< PT.SigmaIntro []
    [e] ->
      return e
    _ ->
      return $ m :< PT.SigmaIntro es

-- -- (e1, ..., en) (n >= 2)
preTermSigmaIntro :: Context m => Parser m PT.PreTerm
preTermSigmaIntro = do
  m <- lift getCurrentHint
  try $ keyword "tuple-new"
  es <- argList preTerm
  return $ m :< PT.SigmaIntro es

preTermNoema :: Context m => Parser m PT.PreTerm
preTermNoema = do
  m <- lift getCurrentHint
  try $ delimiter "&"
  subject <- Ident.fromText <$> symbol
  t <- preTerm
  return $ m :< PT.Noema (m :< PT.Var subject) t

preTermIdealize :: Context m => Parser m PT.PreTerm
preTermIdealize = do
  m <- lift getCurrentHint
  try $ keyword "idealize"
  varList <- manyTill var (keyword "over")
  let varList' = fmap (fmap Ident.fromText) varList
  subject <- Ident.fromText <$> symbol
  e <- doBlock preTerm
  ts <- mapM (\(mx, _) -> lift $ Gensym.newPreAster mx) varList
  return $ m :< PT.NoemaElim subject (castLet subject (zip varList' ts) e)

castLet :: Ident -> [((Hint, Ident), PT.PreTerm)] -> PT.PreTerm -> PT.PreTerm
castLet subject xts cont =
  case xts of
    [] ->
      cont
    ((m, x), t) : rest ->
      bind (m, x, t) (m :< PT.NoemaIntro subject (preVar' m x)) $ castLet subject rest cont

preTermArray :: Context m => Parser m PT.PreTerm
preTermArray = do
  m <- lift getCurrentHint
  try $ keyword "array"
  betweenParen $ do
    elemType <- preTerm
    return $ m :< PT.Array elemType

preTermArrayIntro :: Context m => Parser m PT.PreTerm
preTermArrayIntro = do
  m <- lift getCurrentHint
  try $ keyword "array-new"
  betweenParen $ do
    elems <- sepBy preTerm (delimiter ",")
    return $ m :< PT.ArrayIntro (doNotCare m) elems

preTermArrayAccess :: Context m => Parser m PT.PreTerm
preTermArrayAccess = do
  m <- lift getCurrentHint
  try $ keyword "array-access"
  betweenParen $ do
    array <- preTerm
    delimiter ","
    index <- preTerm
    return $ m :< PT.ArrayAccess (doNotCare m) (doNotCare m) array index

preTermCell :: Context m => Parser m PT.PreTerm
preTermCell = do
  m <- lift getCurrentHint
  try $ keyword "cell"
  betweenParen $ do
    contentType <- preTerm
    return $ m :< PT.Cell contentType

preTermCellIntro :: Context m => Parser m PT.PreTerm
preTermCellIntro = do
  m <- lift getCurrentHint
  try $ keyword "cell-new"
  betweenParen $ do
    content <- preTerm
    return $ m :< PT.CellIntro (doNotCare m) content

preTermCellRead :: Context m => Parser m PT.PreTerm
preTermCellRead = do
  m <- lift getCurrentHint
  try $ keyword "cell-read"
  betweenParen $ do
    cell <- preTerm
    return $ m :< PT.CellRead cell

preTermCellWrite :: Context m => Parser m PT.PreTerm
preTermCellWrite = do
  m <- lift getCurrentHint
  try $ keyword "cell-write"
  betweenParen $ do
    cell <- preTerm
    delimiter ","
    newValue <- preTerm
    return $ m :< PT.CellWrite cell newValue

bind :: BinderF PT.PreTerm -> PT.PreTerm -> PT.PreTerm -> PT.PreTerm
bind mxt@(m, _, _) e cont =
  m :< PT.Let mxt e cont

preTermAdmit :: Context m => Parser m PT.PreTerm
preTermAdmit = do
  m <- lift getCurrentHint
  try $ keyword "admit"
  h <- lift $ Gensym.newPreAster m
  return $
    m
      :< PT.PiElim
        (preVar m "core.os.exit")
        [ h,
          m :< PT.Int (PT.i64 m) 1
        ]

preTermAdmitQuestion :: Context m => Parser m PT.PreTerm
preTermAdmitQuestion = do
  m <- lift getCurrentHint
  try $ keyword "?admit"
  h <- lift $ Gensym.newPreAster m
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

preTermPiElim :: Context m => Parser m PT.PreTerm
preTermPiElim = do
  m <- lift getCurrentHint
  e <- preTermSimple
  impArgs <- impArgList preTerm
  es <- argList preTerm
  ess <- many $ argList preTerm
  if null impArgs
    then return $ foldl' (\base args -> m :< PT.PiElim base args) e $ es : ess
    else do
      f <- lift $ Gensym.newTextualIdentFromText "func"
      h <- lift $ Gensym.newPreAster m
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

preTermPiElimInv :: Context m => Parser m PT.PreTerm
preTermPiElimInv = do
  m <- lift getCurrentHint
  e <- preTermSimple
  f <- betweenBracket preTerm
  fs <- many $ betweenBracket preTerm
  return $ foldl' (\base func -> m :< PT.PiElim func [base]) e $ f : fs

-- --
-- -- term-related helper functions
-- --

preBinder :: Context m => Parser m (BinderF PT.PreTerm)
preBinder =
  choice
    [ try preAscription,
      preAscription'
    ]

preAscription :: Context m => Parser m (BinderF PT.PreTerm)
preAscription = do
  m <- lift getCurrentHint
  x <- symbol
  delimiter ":"
  a <- preTerm

  return (m, Ident.fromText x, a)

typeWithoutIdent :: Context m => Parser m (BinderF PT.PreTerm)
typeWithoutIdent = do
  m <- lift getCurrentHint
  x <- lift $ Gensym.newTextualIdentFromText "_"
  t <- preTerm
  return (m, x, t)

preAscription' :: Context m => Parser m (BinderF PT.PreTerm)
preAscription' = do
  (m, x) <- preSimpleIdent
  h <- lift $ Gensym.newPreAster m
  return (m, x, h)

preSimpleIdent :: Context m => Parser m (Hint, Ident)
preSimpleIdent = do
  m <- lift getCurrentHint
  x <- symbol
  return (m, Ident.fromText x)

preTermIntrospect :: Context m => Parser m PT.PreTerm
preTermIntrospect = do
  m <- lift getCurrentHint
  try $ keyword "introspect"
  key <- symbol
  value <- lift $ getIntrospectiveValue m key
  keyword "with"
  clauseList <- many preTermIntrospectiveClause
  keyword "end"
  case lookup value clauseList of
    Just clause ->
      return clause
    Nothing -> do
      lift $ Throw.raiseError m $ "a clause for `" <> value <> "` is missing"

preTermIntrospectiveClause :: Context m => Parser m (T.Text, PT.PreTerm)
preTermIntrospectiveClause = do
  delimiter "-"
  c <- symbol
  delimiter "->"
  body <- preTerm
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

preTermVar :: Context m => Parser m PT.PreTerm
preTermVar = do
  (m, x) <- var
  return (preVar m x)

preTermText :: Context m => Parser m PT.PreTerm
preTermText = do
  m <- lift getCurrentHint
  try $ keyword "text"
  return $ m :< PT.Text

preTermTextIntro :: Context m => Parser m PT.PreTerm
preTermTextIntro = do
  m <- lift getCurrentHint
  s <- string
  return $ m :< PT.TextIntro s

preTermInteger :: Context m => Parser m PT.PreTerm
preTermInteger = do
  m <- lift getCurrentHint
  intValue <- try integer
  h <- lift $ Gensym.newPreAster m
  return $ m :< PT.Int h intValue

preTermFloat :: Context m => Parser m PT.PreTerm
preTermFloat = do
  m <- lift getCurrentHint
  floatValue <- try float
  h <- lift $ Gensym.newPreAster m
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
