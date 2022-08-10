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
import Data.List
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

--
-- parser for PT.PreTerm
--

preTerm :: Context m => m PT.PreTerm
preTerm = do
  m <- getCurrentHint
  e1 <- preTermEasy
  choice
    [ preTermVoid m e1,
      preTermExplicitAscription m e1,
      return e1
    ]

-- fixme: easy??
preTermEasy :: Context m => m PT.PreTerm
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

preTermSimple :: Context m => m PT.PreTerm
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

preTermLet :: Context m => m PT.PreTerm
preTermLet = do
  m <- getCurrentHint
  try $ keyword "let"
  x <- preTermLetVar
  delimiter "="
  e1 <- preTerm
  keyword "in"
  e2 <- preTerm
  return $ m :< PT.Let x e1 e2

-- let? x : A = e1 in e2
-- let? x     = e1 in e2
preTermLetCoproduct :: Context m => m PT.PreTerm
preTermLetCoproduct = do
  m <- getCurrentHint
  try $ keyword "let?"
  x <- preTermLetVar
  delimiter "="
  e1 <- preTerm
  keyword "in"
  e2 <- preTerm
  err <- Gensym.newTextualIdentFromText "err"
  typeOfLeft <- Gensym.newPreAster m
  typeOfRight <- Gensym.newPreAster m
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

preTermVoid :: Context m => Hint -> PT.PreTerm -> m PT.PreTerm
preTermVoid m e1 = do
  delimiter ";"
  e2 <- preTerm
  f <- Gensym.newTextualIdentFromText "unit"
  return $ bind (m, f, m :< PT.Enum constTop) e1 e2

preTermExplicitAscription :: Context m => Hint -> PT.PreTerm -> m PT.PreTerm
preTermExplicitAscription m e = do
  delimiter ":"
  t <- preTermEasy
  f <- Gensym.newTextualIdentFromText "unit"
  return $ bind (m, f, t) e (m :< PT.Var f)

preTermTau :: Context m => m PT.PreTerm
preTermTau = do
  m <- getCurrentHint
  try $ keyword "tau"
  return $ m :< PT.Tau

preTermAster :: Context m => m PT.PreTerm
preTermAster = do
  m <- getCurrentHint
  delimiter "?"
  Gensym.newPreAster m

preTermPi :: Context m => m PT.PreTerm
preTermPi = do
  m <- getCurrentHint
  domList <- argList $ choice [try preAscription, typeWithoutIdent]
  delimiter "->"
  cod <- preTerm
  return $ m :< PT.Pi domList cod

preTermPiIntro :: Context m => m PT.PreTerm
preTermPiIntro = do
  m <- getCurrentHint
  try $ keyword "lambda"
  varList <- argList preBinder
  e <- choice [preTermDotBind, doBlock preTerm]
  return $ lam m varList e

preTermDotBind :: Context m => m PT.PreTerm
preTermDotBind = do
  delimiter "."
  preTerm

parseDefInfo :: Context m => m PT.DefInfo
parseDefInfo = do
  functionVar <- var
  domInfoList <- argList preBinder
  delimiter ":"
  codType <- preTerm
  e <- asBlock preTerm
  return (functionVar, domInfoList, codType, e)

parseTopDefInfo :: Context m => m PT.TopDefInfo
parseTopDefInfo = do
  m <- getCurrentHint
  funcBaseName <- baseName
  impDomInfoList <- impArgList preBinder
  domInfoList <- argList preBinder
  delimiter ":"
  codType <- preTerm
  e <- asBlock preTerm
  return ((m, funcBaseName), impDomInfoList, domInfoList, codType, e)

-- define name(x1: A1, ..., xn: An)[: A] as e end
preTermPiIntroDef :: Context m => m PT.PreTerm
preTermPiIntroDef = do
  m <- getCurrentHint
  try $ keyword "define"
  ((mFun, functionName), domBinderList, codType, e) <- parseDefInfo
  let piType = mFun :< PT.Pi domBinderList codType
  return $ m :< PT.PiIntro (LamKindFix (mFun, Ident.fromText functionName, piType)) domBinderList e

preTermSigma :: Context m => m PT.PreTerm
preTermSigma = do
  m <- getCurrentHint
  try $ keyword "tuple"
  ts <- argList $ choice [try preAscription, typeWithoutIdent]
  return $ m :< PT.Sigma ts

parseDefiniteDescription :: Context m => m (Hint, GL.GlobalLocator, LL.LocalLocator)
parseDefiniteDescription = do
  m <- getCurrentHint
  globalLocator <- symbol
  globalLocator' <- GL.reflect m globalLocator
  delimiter definiteSep
  localLocator <- parseLocalLocator
  return (m, globalLocator', localLocator)

preTermDefiniteDescription :: Context m => m PT.PreTerm
preTermDefiniteDescription = do
  (m, globalLocator, localLocator) <- try parseDefiniteDescription
  return $ m :< PT.VarGlobal globalLocator localLocator

parseLocalLocator :: Context m => m LL.LocalLocator
parseLocalLocator = do
  m <- getCurrentHint
  rawTxt <- symbol
  LL.reflect m rawTxt

preTermEnumElim :: Context m => m PT.PreTerm
preTermEnumElim = do
  m <- getCurrentHint
  try $ keyword "switch"
  e <- preTerm
  keyword "with"
  clauseList <- many preTermEnumClause
  keyword "end"
  h <- Gensym.newPreAster m
  return $ m :< PT.EnumElim (e, h) clauseList

preTermEnumClause :: Context m => m (PreEnumCase, PT.PreTerm)
preTermEnumClause = do
  m <- getCurrentHint
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
preTermQuestion :: Context m => m PT.PreTerm
preTermQuestion = do
  m <- getCurrentHint
  try $ keyword "question"
  e <- preTerm
  h <- Gensym.newPreAster m
  return $ m :< PT.Question e h

preTermMagic :: Context m => m PT.PreTerm
preTermMagic = do
  m <- getCurrentHint
  try $ keyword "magic"
  choice
    [ preTermMagicCast m,
      preTermMagicStore m,
      preTermMagicLoad m,
      preTermMagicSyscall m,
      preTermMagicExternal m
    ]

preTermMagicBase :: Context m => T.Text -> m PT.PreTerm -> m PT.PreTerm
preTermMagicBase k parser = do
  keyword k
  betweenParen parser

preTermMagicCast :: Context m => Hint -> m PT.PreTerm
preTermMagicCast m = do
  preTermMagicBase "cast" $ do
    castFrom <- preTerm
    castTo <- delimiter "," >> preTerm
    value <- delimiter "," >> preTerm
    return $ m :< PT.Magic (MagicCast castFrom castTo value)

preTermMagicStore :: Context m => Hint -> m PT.PreTerm
preTermMagicStore m = do
  preTermMagicBase "store" $ do
    lt <- lowType
    pointer <- delimiter "," >> preTerm
    value <- delimiter "," >> preTerm
    return $ m :< PT.Magic (MagicStore lt pointer value)

preTermMagicLoad :: Context m => Hint -> m PT.PreTerm
preTermMagicLoad m = do
  preTermMagicBase "load" $ do
    lt <- lowType
    pointer <- delimiter "," >> preTerm
    return $ m :< PT.Magic (MagicLoad lt pointer)

preTermMagicSyscall :: Context m => Hint -> m PT.PreTerm
preTermMagicSyscall m = do
  preTermMagicBase "syscall" $ do
    syscallNum <- integer
    es <- many (delimiter "," >> preTerm)
    return $ m :< PT.Magic (MagicSyscall syscallNum es)

preTermMagicExternal :: Context m => Hint -> m PT.PreTerm
preTermMagicExternal m = do
  preTermMagicBase "external" $ do
    extFunName <- symbol
    es <- many (delimiter "," >> preTerm)
    return $ m :< PT.Magic (MagicExternal (EN.ExternalName extFunName) es)

-- -- t ::= i{n} | f{n} | pointer t | array INT t | struct t ... t
lowType :: Context m => m LowType
lowType = do
  choice
    [ lowTypePointer,
      lowTypeArray,
      lowTypeStruct,
      lowTypeSimple
    ]

lowTypeSimple :: Context m => m LowType
lowTypeSimple =
  choice
    [ betweenParen lowType,
      lowTypeNumber
    ]

lowTypePointer :: Context m => m LowType
lowTypePointer = do
  keyword "pointer"
  lowTypeSimple

lowTypeArray :: Context m => m LowType
lowTypeArray = do
  keyword "array"
  intValue <- integer
  LowTypeArray (fromInteger intValue) <$> lowTypeSimple

lowTypeStruct :: Context m => m LowType
lowTypeStruct = do
  keyword "struct"
  LowTypeStruct <$> many lowTypeSimple

lowTypeNumber :: Context m => m LowType
lowTypeNumber = do
  sizeString <- symbol
  case fromText sizeString of
    Just primNum ->
      return $ LowTypePrimNum primNum
    _ -> do
      sizeString' <- asTokens sizeString
      labelInteger <- asLabel "i{n}"
      labelFloat <- asLabel "f{n}"
      failure (Just sizeString') [labelInteger, labelFloat]

preTermMatch :: Context m => m PT.PreTerm
preTermMatch = do
  m <- getCurrentHint
  try $ keyword "match"
  e <- preTerm
  clauseList <- withBlock $ manyList preTermMatchClause
  return $ m :< PT.Match Nothing (e, doNotCare m) clauseList

preTermMatchNoetic :: Context m => m PT.PreTerm
preTermMatchNoetic = do
  m <- getCurrentHint
  try $ keyword "match-noetic"
  e <- preTerm
  keyword "with"
  s <- Gensym.newPreAster m
  t <- Gensym.newPreAster m
  let e' = castFromNoema s t e
  clauseList <- manyList preTermMatchClause
  keyword "end"
  let clauseList' = map (modifyPrePattern s) clauseList
  return $ m :< PT.Match (Just s) (e', doNotCare m) clauseList'

preTermMatchClause :: Context m => m (PrePatternF PT.PreTerm, PT.PreTerm)
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

preTermPattern :: Context m => m (PrePatternF PT.PreTerm)
preTermPattern = do
  m <- getCurrentHint
  c <- symbol
  patArgs <- argList preBinder
  return (m, Left $ UN.UnresolvedName c, patArgs)

-- let (x1 : A1, ..., xn : An) = e1 in e2
preTermLetSigmaElim :: Context m => m PT.PreTerm
preTermLetSigmaElim = do
  m <- getCurrentHint
  try $ keyword "let"
  -- xts <- parseArgList2 preBinder
  xts <- argList preBinder
  delimiter "="
  e1 <- preTerm
  keyword "in"
  e2 <- preTerm
  return $ m :< PT.SigmaElim xts e1 e2

preTermLetVar :: Context m => m (BinderF PT.PreTerm)
preTermLetVar = do
  m <- getCurrentHint
  choice
    [ try $ do
        x <- symbol
        delimiter ":"
        a <- preTerm
        return (m, Ident.fromText x, a),
      do
        x <- symbol
        h <- Gensym.newPreAster m
        return (m, Ident.fromText x, h)
    ]

preTermIf :: Context m => m PT.PreTerm
preTermIf = do
  m <- getCurrentHint
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

foldIf :: Context m => Hint -> PT.PreTerm -> PT.PreTerm -> [(PT.PreTerm, PT.PreTerm)] -> PT.PreTerm -> m PT.PreTerm
foldIf m ifCond ifBody elseIfList elseBody =
  case elseIfList of
    [] -> do
      h <- Gensym.newPreAster m
      return $
        m
          :< PT.EnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel (weakenEnumLabel enumLabelBoolTrue), ifBody),
              (m :< EnumCaseLabel (weakenEnumLabel enumLabelBoolFalse), elseBody)
            ]
    ((elseIfCond, elseIfBody) : rest) -> do
      cont <- foldIf m elseIfCond elseIfBody rest elseBody
      h <- Gensym.newPreAster m
      return $
        m
          :< PT.EnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel (weakenEnumLabel enumLabelBoolTrue), ifBody),
              (m :< EnumCaseLabel (weakenEnumLabel enumLabelBoolFalse), cont)
            ]

preTermParen :: Context m => m PT.PreTerm
preTermParen = do
  m <- getCurrentHint
  es <- argList preTerm
  case es of
    [] ->
      return $ m :< PT.SigmaIntro []
    [e] ->
      return e
    _ ->
      return $ m :< PT.SigmaIntro es

-- -- (e1, ..., en) (n >= 2)
preTermSigmaIntro :: Context m => m PT.PreTerm
preTermSigmaIntro = do
  m <- getCurrentHint
  try $ keyword "tuple-new"
  es <- argList preTerm
  return $ m :< PT.SigmaIntro es

preTermNoema :: Context m => m PT.PreTerm
preTermNoema = do
  m <- getCurrentHint
  try $ delimiter "&"
  subject <- Ident.fromText <$> symbol
  t <- preTerm
  return $ m :< PT.Noema (m :< PT.Var subject) t

preTermIdealize :: Context m => m PT.PreTerm
preTermIdealize = do
  m <- getCurrentHint
  try $ keyword "idealize"
  varList <- manyTill var (keyword "over")
  let varList' = fmap (fmap Ident.fromText) varList
  subject <- Ident.fromText <$> symbol
  e <- doBlock preTerm
  ts <- mapM (\(mx, _) -> Gensym.newPreAster mx) varList
  return $ m :< PT.NoemaElim subject (castLet subject (zip varList' ts) e)

castLet :: Ident -> [((Hint, Ident), PT.PreTerm)] -> PT.PreTerm -> PT.PreTerm
castLet subject xts cont =
  case xts of
    [] ->
      cont
    ((m, x), t) : rest ->
      bind (m, x, t) (m :< PT.NoemaIntro subject (preVar' m x)) $ castLet subject rest cont

preTermArray :: Context m => m PT.PreTerm
preTermArray = do
  m <- getCurrentHint
  try $ keyword "array"
  betweenParen $ do
    elemType <- preTerm
    return $ m :< PT.Array elemType

preTermArrayIntro :: Context m => m PT.PreTerm
preTermArrayIntro = do
  m <- getCurrentHint
  try $ keyword "array-new"
  betweenParen $ do
    elems <- sepBy preTerm (delimiter ",")
    return $ m :< PT.ArrayIntro (doNotCare m) elems

preTermArrayAccess :: Context m => m PT.PreTerm
preTermArrayAccess = do
  m <- getCurrentHint
  try $ keyword "array-access"
  betweenParen $ do
    array <- preTerm
    delimiter ","
    index <- preTerm
    return $ m :< PT.ArrayAccess (doNotCare m) (doNotCare m) array index

preTermCell :: Context m => m PT.PreTerm
preTermCell = do
  m <- getCurrentHint
  try $ keyword "cell"
  betweenParen $ do
    contentType <- preTerm
    return $ m :< PT.Cell contentType

preTermCellIntro :: Context m => m PT.PreTerm
preTermCellIntro = do
  m <- getCurrentHint
  try $ keyword "cell-new"
  betweenParen $ do
    content <- preTerm
    return $ m :< PT.CellIntro (doNotCare m) content

preTermCellRead :: Context m => m PT.PreTerm
preTermCellRead = do
  m <- getCurrentHint
  try $ keyword "cell-read"
  betweenParen $ do
    cell <- preTerm
    return $ m :< PT.CellRead cell

preTermCellWrite :: Context m => m PT.PreTerm
preTermCellWrite = do
  m <- getCurrentHint
  try $ keyword "cell-write"
  betweenParen $ do
    cell <- preTerm
    delimiter ","
    newValue <- preTerm
    return $ m :< PT.CellWrite cell newValue

bind :: BinderF PT.PreTerm -> PT.PreTerm -> PT.PreTerm -> PT.PreTerm
bind mxt@(m, _, _) e cont =
  m :< PT.Let mxt e cont

preTermAdmit :: Context m => m PT.PreTerm
preTermAdmit = do
  m <- getCurrentHint
  try $ keyword "admit"
  h <- Gensym.newPreAster m
  return $
    m
      :< PT.PiElim
        (preVar m "core.os.exit")
        [ h,
          m :< PT.Int (PT.i64 m) 1
        ]

preTermAdmitQuestion :: Context m => m PT.PreTerm
preTermAdmitQuestion = do
  m <- getCurrentHint
  try $ keyword "?admit"
  h <- Gensym.newPreAster m
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

preTermPiElim :: Context m => m PT.PreTerm
preTermPiElim = do
  m <- getCurrentHint
  e <- preTermSimple
  impArgs <- impArgList preTerm
  es <- argList preTerm
  ess <- many $ argList preTerm
  if null impArgs
    then return $ foldl' (\base args -> m :< PT.PiElim base args) e $ es : ess
    else do
      f <- Gensym.newTextualIdentFromText "func"
      h <- Gensym.newPreAster m
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

preTermPiElimInv :: Context m => m PT.PreTerm
preTermPiElimInv = do
  m <- getCurrentHint
  e <- preTermSimple
  f <- betweenBracket preTerm
  fs <- many $ betweenBracket preTerm
  return $ foldl' (\base func -> m :< PT.PiElim func [base]) e $ f : fs

-- --
-- -- term-related helper functions
-- --

preBinder :: Context m => m (BinderF PT.PreTerm)
preBinder =
  choice
    [ try preAscription,
      preAscription'
    ]

preAscription :: Context m => m (BinderF PT.PreTerm)
preAscription = do
  m <- getCurrentHint
  x <- symbol
  delimiter ":"
  a <- preTerm

  return (m, Ident.fromText x, a)

typeWithoutIdent :: Context m => m (BinderF PT.PreTerm)
typeWithoutIdent = do
  m <- getCurrentHint
  x <- Gensym.newTextualIdentFromText "_"
  t <- preTerm
  return (m, x, t)

preAscription' :: Context m => m (BinderF PT.PreTerm)
preAscription' = do
  (m, x) <- preSimpleIdent
  h <- Gensym.newPreAster m
  return (m, x, h)

preSimpleIdent :: Context m => m (Hint, Ident)
preSimpleIdent = do
  m <- getCurrentHint
  x <- symbol
  return (m, Ident.fromText x)

preTermIntrospect :: Context m => m PT.PreTerm
preTermIntrospect = do
  m <- getCurrentHint
  try $ keyword "introspect"
  key <- symbol
  value <- getIntrospectiveValue m key
  keyword "with"
  clauseList <- many preTermIntrospectiveClause
  keyword "end"
  case lookup value clauseList of
    Just clause ->
      return clause
    Nothing -> do
      Throw.raiseError m $ "a clause for `" <> value <> "` is missing"

preTermIntrospectiveClause :: Context m => m (T.Text, PT.PreTerm)
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

preTermVar :: Context m => m PT.PreTerm
preTermVar = do
  (m, x) <- var
  return (preVar m x)

preTermText :: Context m => m PT.PreTerm
preTermText = do
  m <- getCurrentHint
  try $ keyword "text"
  return $ m :< PT.Text

preTermTextIntro :: Context m => m PT.PreTerm
preTermTextIntro = do
  m <- getCurrentHint
  s <- string
  return $ m :< PT.TextIntro s

preTermInteger :: Context m => m PT.PreTerm
preTermInteger = do
  m <- getCurrentHint
  intValue <- try integer
  h <- Gensym.newPreAster m
  return $ m :< PT.Int h intValue

preTermFloat :: Context m => m PT.PreTerm
preTermFloat = do
  m <- getCurrentHint
  floatValue <- try float
  h <- Gensym.newPreAster m
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
