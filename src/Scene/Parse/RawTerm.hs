module Scene.Parse.RawTerm
  ( rawExpr,
    rawTerm,
    preAscription,
    preBinder,
    parseTopDefInfo,
    typeWithoutIdent,
    preVar,
    parseName,
  )
where

import Context.App
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Trans
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.Annotation qualified as AN
import Entity.Arch qualified as Arch
import Entity.Const
import Entity.DefiniteDescription qualified as DD
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.Hint.Reify
import Entity.Key
import Entity.Locator qualified as L
import Entity.LowType qualified as LT
import Entity.Magic qualified as M
import Entity.Name
import Entity.Noema qualified as N
import Entity.OS qualified as OS
import Entity.Opacity qualified as O
import Entity.PrimType qualified as PT
import Entity.PrimType.FromText qualified as PT
import Entity.RawBinder
import Entity.RawIdent
import Entity.RawLamKind qualified as LK
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.Remark
import Entity.TargetPlatform qualified as TP
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Scene.Parse.Core
import Text.Megaparsec

--
-- parser for RT.RawTerm
--

rawExpr :: Parser RT.RawTerm
rawExpr = do
  m <- getCurrentHint
  choice
    [ rawExprLet m,
      rawTermBind m,
      rawExprSeqOrTerm m
    ]

rawExprLet :: Hint -> Parser RT.RawTerm
rawExprLet m = do
  choice
    [ try $ rawTermLetOption m,
      try $ rawTermLetCoproduct m,
      rawTermLetOrLetOn m
    ]

rawExprSeqOrTerm :: Hint -> Parser RT.RawTerm
rawExprSeqOrTerm m = do
  e1 <- rawTerm
  choice
    [ do
        e2 <- rawExpr
        f <- lift Gensym.newTextForHole
        top <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreTop
        return $ bind (m, f, top) e1 e2,
      return e1
    ]

rawTerm :: Parser RT.RawTerm
rawTerm = do
  choice
    [ rawTermPiIntro,
      rawTermPiIntroDef,
      rawTermIntrospect,
      rawTermMagic,
      rawTermMatchNoetic,
      rawTermMatch,
      rawTermFlow,
      rawTermFlowIntro,
      rawTermFlowElim,
      rawTermIf,
      rawTermListIntro,
      rawTermPiGeneral,
      try rawTermPiElimByKey,
      rawTermPiOrConsOrAscOrBasic
    ]

rawTermBasic :: Parser RT.RawTerm
rawTermBasic = do
  choice
    [ rawTermNoema,
      rawTermOption,
      rawTermEmbody,
      rawTermTuple,
      rawTermPiElimOrSimple
    ]

rawTermSimple :: Parser RT.RawTerm
rawTermSimple = do
  choice
    [ rawTermBrace,
      rawTermTextIntro,
      rawTermTau,
      rawTermAdmit,
      rawTermHole,
      rawTermInteger,
      rawTermFloat,
      rawTermPiElimOrSymbol
    ]

rawTermLetOrLetOn :: Hint -> Parser RT.RawTerm
rawTermLetOrLetOn m = do
  keyword "let"
  pat@(mx, _) <- rawTermPattern
  (x, modifier) <- getContinuationModifier pat
  t <- rawTermLetVarAscription mx
  let mxt = (mx, x, t)
  choice
    [ do
        keyword "on"
        noeticVarList <- commaList rawTermNoeticVar
        lift $ ensureNoeticVarLinearity m S.empty $ map snd noeticVarList
        delimiter "="
        e1 <- rawTerm
        e2 <- rawExpr
        return $ m :< RT.Let mxt noeticVarList e1 (modifier False e2),
      do
        delimiter "="
        e1 <- rawTerm
        e2 <- rawExpr
        return $ m :< RT.Let mxt [] e1 (modifier False e2)
    ]

getContinuationModifier :: (Hint, RP.RawPattern) -> Parser (RawIdent, N.IsNoetic -> RT.RawTerm -> RT.RawTerm)
getContinuationModifier pat@(m, _) =
  case pat of
    (_, RP.Var (Var x)) ->
      return (x, \_ e -> e)
    _ -> do
      tmp <- lift $ Gensym.newTextFromText "tmp"
      return (tmp, \isNoetic e -> m :< RT.DataElim isNoetic [m :< RT.Var (Var tmp)] (RP.new [(V.fromList [pat], e)]))

rawTermLetVarAscription :: Hint -> Parser RT.RawTerm
rawTermLetVarAscription m = do
  mt <- rawTermLetVarAscription'
  case mt of
    Just t ->
      return t
    Nothing ->
      lift $ Gensym.newPreHole m

ascribe :: Hint -> RT.RawTerm -> RT.RawTerm -> Parser RT.RawTerm
ascribe m t e = do
  tmp <- lift $ Gensym.newTextFromText "tmp"
  return $ bind (m, tmp, t) e (m :< RT.Var (Var tmp))

rawTermLetVarAscription' :: Parser (Maybe RT.RawTerm)
rawTermLetVarAscription' =
  choice
    [ try $ do
        delimiter ":"
        Just <$> rawTerm,
      return Nothing
    ]

rawTermBind :: Hint -> Parser RT.RawTerm
rawTermBind m = do
  keyword "bind"
  pat@(mx, _) <- rawTermPattern
  (x, modifier) <- getContinuationModifier pat
  t <- rawTermLetVarAscription mx
  delimiter "="
  e1 <- rawTerm
  e2 <- rawExpr
  return $ m :< RT.Let (mx, x, t) [] e1 (modifier True e2)

ensureNoeticVarLinearity :: Hint -> S.Set RawIdent -> [RawIdent] -> App ()
ensureNoeticVarLinearity m foundVarSet vs =
  case vs of
    [] ->
      return ()
    name : rest
      | S.member name foundVarSet ->
          Throw.raiseError m $ "found a non-linear occurrence of `" <> name <> "`."
      | otherwise ->
          ensureNoeticVarLinearity m (S.insert name foundVarSet) rest

rawTermNoeticVar :: Parser (Hint, T.Text)
rawTermNoeticVar = do
  (m, x) <- var
  return (m, x)

rawTermLetOption :: Hint -> Parser RT.RawTerm
rawTermLetOption m = do
  keyword "let?"
  pat@(mx, _) <- rawTermPattern
  resultType <- rawTermLetVarAscription mx
  delimiter "="
  e1 <- rawTerm
  optionTypeDD <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreOption
  let optionType = m :< RT.PiElim optionTypeDD [resultType]
  e1' <- ascribe m optionType e1
  e2 <- rawExpr
  (optionNoneGL, optionNoneLL) <- lift $ Throw.liftEither $ DD.getLocatorPair m coreOptionNone
  optionNoneVar <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreOptionNone
  optionSome <- lift $ handleDefiniteDescriptionIntoRawConsName m coreOptionSome
  return $
    m
      :< RT.DataElim
        False
        [e1']
        ( RP.new
            [ (V.fromList [(m, RP.Var $ Locator (optionNoneGL, optionNoneLL))], optionNoneVar),
              (V.fromList [(m, RP.Cons optionSome [pat])], e2)
            ]
        )

-- let+ x = e1 in e2
rawTermLetCoproduct :: Hint -> Parser RT.RawTerm
rawTermLetCoproduct m = do
  keyword "let+"
  pat@(mx, _) <- rawTermPattern
  rightType <- rawTermLetVarAscription mx
  delimiter "="
  e1 <- rawTerm
  sumTypeDD <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreSum
  leftType <- lift $ Gensym.newPreHole m
  let sumType = m :< RT.PiElim sumTypeDD [leftType, rightType]
  e1' <- ascribe m sumType e1
  e2 <- rawExpr
  err <- lift Gensym.newText
  sumLeft <- lift $ handleDefiniteDescriptionIntoRawConsName m coreSumLeft
  sumRight <- lift $ handleDefiniteDescriptionIntoRawConsName m coreSumRight
  sumLeftVar <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreSumLeft
  return $
    m
      :< RT.DataElim
        False
        [e1']
        ( RP.new
            [ (V.fromList [(m, RP.Cons sumLeft [(m, RP.Var (Var err))])], m :< RT.PiElim sumLeftVar [preVar m err]),
              (V.fromList [(m, RP.Cons sumRight [pat])], e2)
            ]
        )

rawTermEmbody :: Parser RT.RawTerm
rawTermEmbody = do
  m <- getCurrentHint
  delimiter "!"
  e <- rawTermBasic
  return $ m :< RT.Embody e

rawTermTau :: Parser RT.RawTerm
rawTermTau = do
  m <- getCurrentHint
  keyword "tau"
  return $ m :< RT.Tau

rawTermHole :: Parser RT.RawTerm
rawTermHole = do
  m <- getCurrentHint
  keyword "_"
  lift $ Gensym.newPreHole m

rawTermPiGeneral :: Parser RT.RawTerm
rawTermPiGeneral = do
  m <- getCurrentHint
  domList <- argList $ choice [try preAscription, typeWithoutIdent]
  delimiter "->"
  cod <- rawTerm
  return $ m :< RT.Pi domList cod

rawTermPiOrConsOrAscOrBasic :: Parser RT.RawTerm
rawTermPiOrConsOrAscOrBasic = do
  m <- getCurrentHint
  basic <- rawTermBasic
  choice
    [ do
        delimiter "->"
        x <- lift Gensym.newTextForHole
        cod <- rawTerm
        return $ m :< RT.Pi [(m, x, basic)] cod,
      do
        delimiter ":<"
        rest <- rawTerm
        listCons <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreListCons
        return $ m :< RT.PiElim listCons [basic, rest],
      do
        delimiter "*"
        ts <- sepBy1 rawTermBasic (delimiter "*")
        return $ foldByOp m (Var "product") (basic : ts),
      do
        delimiter ":"
        t <- rawTerm
        ascribe m t basic,
      return basic
    ]

foldByOp :: Hint -> Name -> [RT.RawTerm] -> RT.RawTerm
foldByOp m op es =
  case es of
    [] ->
      error "RawTerm.foldProduct: invalid argument"
    [e] ->
      e
    e : rest ->
      m :< RT.PiElim (m :< RT.Var op) [e, foldByOp m op rest]

rawTermPiIntro :: Parser RT.RawTerm
rawTermPiIntro = do
  m <- getCurrentHint
  keyword "lambda"
  varList <- argList preBinder
  e <- betweenBrace rawExpr
  return $ lam m varList e

parseDefInfo :: Hint -> Parser RT.DefInfo
parseDefInfo m = do
  functionVar <- var
  domInfoList <- argList preBinder
  codType <- parseDefInfoCod m
  e <- betweenBrace rawExpr
  return (functionVar, domInfoList, codType, e)

parseTopDefInfo :: Parser RT.TopDefInfo
parseTopDefInfo = do
  m <- getCurrentHint
  funcBaseName <- baseName
  impDomInfoList <- impArgList preBinder
  domInfoList <- argList preBinder
  codType <- parseDefInfoCod m
  e <- betweenBrace rawExpr
  return ((m, funcBaseName), impDomInfoList, domInfoList, codType, e)

parseDefInfoCod :: Hint -> Parser RT.RawTerm
parseDefInfoCod m =
  choice
    [ do
        delimiter ":"
        rawTerm,
      lift $ Gensym.newPreHole m
    ]

-- define name(x1: A1, ..., xn: An)[: A] as e end
rawTermPiIntroDef :: Parser RT.RawTerm
rawTermPiIntroDef = do
  m <- getCurrentHint
  keyword "define"
  ((mFun, functionName), domBinderList, codType, e) <- parseDefInfo m
  return $ m :< RT.PiIntro (LK.Fix (mFun, functionName, codType)) domBinderList e

rawTermMagic :: Parser RT.RawTerm
rawTermMagic = do
  m <- getCurrentHint
  keyword "magic"
  choice
    [ rawTermMagicCast m,
      rawTermMagicStore m,
      rawTermMagicLoad m,
      rawTermMagicSyscall m,
      rawTermMagicExternal m
    ]

rawTermMagicBase :: T.Text -> Parser a -> Parser a
rawTermMagicBase k parser = do
  keyword k
  betweenParen parser

rawTermMagicCast :: Hint -> Parser RT.RawTerm
rawTermMagicCast m = do
  rawTermMagicBase "cast" $ do
    castFrom <- rawTerm
    castTo <- delimiter "," >> rawTerm
    value <- delimiter "," >> rawTerm
    return $ m :< RT.Magic (M.Cast castFrom castTo value)

rawTermMagicStore :: Hint -> Parser RT.RawTerm
rawTermMagicStore m = do
  rawTermMagicBase "store" $ do
    lt <- lowType
    pointer <- delimiter "," >> rawTerm
    value <- delimiter "," >> rawTerm
    return $ m :< RT.Magic (M.Store lt pointer value)

rawTermMagicLoad :: Hint -> Parser RT.RawTerm
rawTermMagicLoad m = do
  rawTermMagicBase "load" $ do
    lt <- lowType
    pointer <- delimiter "," >> rawTerm
    return $ m :< RT.Magic (M.Load lt pointer)

rawTermMagicSyscall :: Hint -> Parser RT.RawTerm
rawTermMagicSyscall m = do
  rawTermMagicBase "syscall" $ do
    syscallNum <- integer
    es <- many (delimiter "," >> rawTerm)
    return $ m :< RT.Magic (M.Syscall syscallNum es)

rawTermMagicExternal :: Hint -> Parser RT.RawTerm
rawTermMagicExternal m = do
  rawTermMagicBase "external" $ do
    extFunName <- symbol
    es <- many (delimiter "," >> rawTerm)
    return $ m :< RT.Magic (M.External (EN.ExternalName extFunName) es)

-- -- t ::= i{n} | f{n} | pointer t | array INT t | struct t ... t
lowType :: Parser LT.LowType
lowType = do
  choice
    [ lowTypePointer,
      lowTypeArray,
      lowTypeStruct,
      lowTypeNumber
    ]

lowTypePointer :: Parser LT.LowType
lowTypePointer = do
  keyword "pointer"
  LT.Pointer <$> betweenParen lowType

lowTypeArray :: Parser LT.LowType
lowTypeArray = do
  keyword "array"
  betweenParen $ do
    intValue <- integer
    delimiter ","
    LT.Array (fromInteger intValue) <$> lowType

lowTypeStruct :: Parser LT.LowType
lowTypeStruct = do
  keyword "struct"
  LT.Struct <$> argList lowType

lowTypeNumber :: Parser LT.LowType
lowTypeNumber = do
  LT.PrimNum <$> primType

primType :: Parser PT.PrimType
primType = do
  m <- getCurrentHint
  sizeString <- symbol
  dataSize <- lift $ Env.getDataSize m
  case PT.fromText dataSize sizeString of
    Just primNum ->
      return primNum
    _ -> do
      failure (Just (asTokens sizeString)) (S.fromList [asLabel "i{n}", asLabel "f{n}"])

rawTermMatch :: Parser RT.RawTerm
rawTermMatch = do
  m <- getCurrentHint
  keyword "match"
  es <- commaList rawTermBasic
  patternRowList <- betweenBrace $ manyList $ rawTermPatternRow (length es)
  return $ m :< RT.DataElim False es (RP.new patternRowList)

rawTermMatchNoetic :: Parser RT.RawTerm
rawTermMatchNoetic = do
  m <- getCurrentHint
  keyword "case"
  es <- commaList rawTermBasic
  patternRowList <- betweenBrace $ manyList $ rawTermPatternRow (length es)
  return $ m :< RT.DataElim True es (RP.new patternRowList)

rawTermPatternRow :: Int -> Parser (RP.RawPatternRow RT.RawTerm)
rawTermPatternRow patternSize = do
  m <- getCurrentHint
  patternList <- commaList rawTermPattern
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
  delimiter "=>"
  body <- rawExpr
  return (V.fromList patternList, body)

rawTermPattern :: Parser (Hint, RP.RawPattern)
rawTermPattern = do
  m <- getCurrentHint
  headPat <- rawTermPatternBasic
  choice
    [ try $ do
        delimiter ":<"
        pat <- rawTermPattern
        listCons <- lift $ handleDefiniteDescriptionIntoRawConsName m coreListCons
        return (m, RP.Cons listCons [headPat, pat]),
      return headPat
    ]

rawTermPatternBasic :: Parser (Hint, RP.RawPattern)
rawTermPatternBasic =
  choice
    [ rawTermPatternListIntro,
      try rawTermPatternProductIntro,
      rawTermPatternConsOrVar
    ]

rawTermPatternListIntro :: Parser (Hint, RP.RawPattern)
rawTermPatternListIntro = do
  m <- getCurrentHint
  patList <- betweenBracket $ commaList rawTermPattern
  listNil <- lift $ Throw.liftEither $ DD.getLocatorPair m coreListNil
  listCons <- lift $ handleDefiniteDescriptionIntoRawConsName m coreListCons
  return $ foldListAppPat m listNil listCons patList

foldListAppPat ::
  Hint ->
  L.Locator ->
  Name ->
  [(Hint, RP.RawPattern)] ->
  (Hint, RP.RawPattern)
foldListAppPat m listNil listCons es =
  case es of
    [] ->
      (m, RP.Var $ Locator listNil)
    e : rest -> do
      let rest' = foldListAppPat m listNil listCons rest
      (m, RP.Cons listCons [e, rest'])

rawTermPatternProductIntro :: Parser (Hint, RP.RawPattern)
rawTermPatternProductIntro = do
  m <- getCurrentHint
  keyword "tuple"
  patList <- betweenParen $ commaList rawTermPattern
  topUnit <- lift $ Throw.liftEither $ DD.getLocatorPair m coreTopUnit
  return $ foldTuplePat m (Locator topUnit) patList

foldTuplePat :: Hint -> Name -> [(Hint, RP.RawPattern)] -> (Hint, RP.RawPattern)
foldTuplePat m topUnit es =
  case es of
    [] ->
      (m, RP.Var topUnit)
    [e] ->
      e
    e : rest -> do
      let rest' = foldTuplePat m topUnit rest
      (m, RP.Cons (Var "Product") [e, rest'])

parseName :: Parser (Hint, Name)
parseName = do
  (m, varText) <- var
  case DD.getLocatorPair m varText of
    Left _ ->
      return (m, Var varText)
    Right (gl, ll) ->
      return (m, Locator (gl, ll))

rawTermPatternConsOrVar :: Parser (Hint, RP.RawPattern)
rawTermPatternConsOrVar = do
  (m, varOrLocator) <- parseName
  choice
    [ do
        patArgs <- argList rawTermPattern
        return (m, RP.Cons varOrLocator patArgs),
      do
        return (m, RP.Var varOrLocator)
    ]

rawTermIf :: Parser RT.RawTerm
rawTermIf = do
  m <- getCurrentHint
  keyword "if"
  ifCond <- rawTerm
  ifBody <- betweenBrace rawExpr
  elseIfList <- many $ do
    keyword "else-if"
    elseIfCond <- rawTerm
    elseIfBody <- betweenBrace rawExpr
    return (elseIfCond, elseIfBody)
  keyword "else"
  elseBody <- betweenBrace rawExpr
  boolTrue <- lift $ Throw.liftEither $ DD.getLocatorPair m coreBoolTrue
  boolFalse <- lift $ Throw.liftEither $ DD.getLocatorPair m coreBoolFalse
  return $ foldIf m (Locator boolTrue) (Locator boolFalse) ifCond ifBody elseIfList elseBody

foldIf ::
  Hint ->
  Name ->
  Name ->
  RT.RawTerm ->
  RT.RawTerm ->
  [(RT.RawTerm, RT.RawTerm)] ->
  RT.RawTerm ->
  RT.RawTerm
foldIf m true false ifCond@(mIf :< _) ifBody elseIfList elseBody =
  case elseIfList of
    [] -> do
      m
        :< RT.DataElim
          False
          [ifCond]
          ( RP.new
              [ (V.fromList [(mIf, RP.Var true)], ifBody),
                (V.fromList [(mIf, RP.Var false)], elseBody)
              ]
          )
    ((elseIfCond, elseIfBody) : rest) -> do
      let cont = foldIf m true false elseIfCond elseIfBody rest elseBody
      m
        :< RT.DataElim
          False
          [ifCond]
          ( RP.new
              [ (V.fromList [(mIf, RP.Var true)], ifBody),
                (V.fromList [(mIf, RP.Var false)], cont)
              ]
          )

rawTermBrace :: Parser RT.RawTerm
rawTermBrace =
  betweenBrace rawExpr

rawTermTuple :: Parser RT.RawTerm
rawTermTuple = do
  m <- getCurrentHint
  keyword "tuple"
  es <- betweenParen $ commaList rawExpr
  case es of
    [] ->
      return $ m :< RT.Var (Var "top.Unit")
    [e] ->
      return e
    _ ->
      return $ foldByOp m (Var "Product") es

bind :: RawBinder RT.RawTerm -> RT.RawTerm -> RT.RawTerm -> RT.RawTerm
bind mxt@(m, _, _) e cont =
  m :< RT.Let mxt [] e cont

rawTermNoema :: Parser RT.RawTerm
rawTermNoema = do
  m <- getCurrentHint
  delimiter "&"
  t <- rawTermBasic
  return $ m :< RT.Noema t

rawTermFlow :: Parser RT.RawTerm
rawTermFlow = do
  m <- getCurrentHint
  keyword "flow"
  flowVar <- lift $ Throw.liftEither $ DD.getLocatorPair m coreThreadFlowInner
  t <- betweenParen rawTerm
  return $ m :< RT.Flow flowVar t

rawTermFlowIntro :: Parser RT.RawTerm
rawTermFlowIntro = do
  m <- getCurrentHint
  keyword "detach"
  flowVar <- lift $ Throw.liftEither $ DD.getLocatorPair m coreThreadFlowInner
  detachVar <- lift $ Throw.liftEither $ DD.getLocatorPair m coreThreadDetach
  e <- betweenBrace rawExpr
  return $ m :< RT.FlowIntro flowVar detachVar (m :< RT.PiIntro (LK.Normal O.Opaque) [] e)

rawTermFlowElim :: Parser RT.RawTerm
rawTermFlowElim = do
  m <- getCurrentHint
  keyword "attach"
  flowVar <- lift $ Throw.liftEither $ DD.getLocatorPair m coreThreadFlowInner
  attachVar <- lift $ Throw.liftEither $ DD.getLocatorPair m coreThreadAttach
  e <- rawTerm
  return $ m :< RT.FlowElim flowVar attachVar e

rawTermOption :: Parser RT.RawTerm
rawTermOption = do
  m <- getCurrentHint
  delimiter "?"
  t <- rawTermBasic
  return $ m :< RT.PiElim (m :< RT.Var (Var "option")) [t]

rawTermAdmit :: Parser RT.RawTerm
rawTermAdmit = do
  m <- getCurrentHint
  keyword "admit"
  admit <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreSystemAdmit
  textType <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreText
  return $
    m
      :< RT.Annotation
        Warning
        (AN.Type ())
        ( m
            :< RT.PiElim
              admit
              [m :< RT.Prim (WP.Value (WPV.StaticText textType ("admit: " <> T.pack (toString m) <> "\n")))]
        )

rawTermPiElimOrSimple :: Parser RT.RawTerm
rawTermPiElimOrSimple = do
  m <- getCurrentHint
  e <- rawTermSimple
  elems <- many $ argList rawTerm
  foldPiElim m e elems

rawTermPiElimByKey :: Parser RT.RawTerm
rawTermPiElimByKey = do
  m <- getCurrentHint
  name <- snd <$> parseName
  rowList <- betweenBrace $ manyList rawTermKeyValuePair
  return $ m :< RT.PiElimByKey name rowList

rawTermKeyValuePair :: Parser (Hint, Key, RT.RawTerm)
rawTermKeyValuePair = do
  (m, key) <- var
  delimiter "<="
  value <- rawExpr
  return (m, key, value)

foldPiElim :: Hint -> RT.RawTerm -> [[RT.RawTerm]] -> Parser RT.RawTerm
foldPiElim m e elemList =
  case elemList of
    [] ->
      return e
    args : rest ->
      foldPiElim m (m :< RT.PiElim e args) rest

--
-- term-related helper functions
--

preBinder :: Parser (RawBinder RT.RawTerm)
preBinder =
  choice
    [ try preAscription,
      preAscription'
    ]

preAscription :: Parser (RawBinder RT.RawTerm)
preAscription = do
  (m, x) <- var
  delimiter ":"
  a <- rawTerm
  return (m, x, a)

typeWithoutIdent :: Parser (RawBinder RT.RawTerm)
typeWithoutIdent = do
  m <- getCurrentHint
  x <- lift Gensym.newTextForHole
  t <- rawTerm
  return (m, x, t)

preAscription' :: Parser (RawBinder RT.RawTerm)
preAscription' = do
  (m, x) <- var
  h <- lift $ Gensym.newPreHole m
  return (m, x, h)

rawTermListIntro :: Parser RT.RawTerm
rawTermListIntro = do
  m <- getCurrentHint
  es <- betweenBracket $ commaList rawTerm
  listNil <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreListNil
  listCons <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreListCons
  return $ foldListApp m listNil listCons es

foldListApp :: Hint -> RT.RawTerm -> RT.RawTerm -> [RT.RawTerm] -> RT.RawTerm
foldListApp m listNil listCons es =
  case es of
    [] ->
      listNil
    e : rest ->
      m :< RT.PiElim listCons [e, foldListApp m listNil listCons rest]

rawTermIntrospect :: Parser RT.RawTerm
rawTermIntrospect = do
  m <- getCurrentHint
  keyword "introspect"
  key <- symbol
  value <- lift $ getIntrospectiveValue m key
  clauseList <- betweenBrace $ manyList rawTermIntrospectiveClause
  case lookup value clauseList of
    Just clause ->
      return clause
    Nothing -> do
      lift $ Throw.raiseError m $ "this term doesn't support `" <> value <> "`."

rawTermIntrospectiveClause :: Parser (T.Text, RT.RawTerm)
rawTermIntrospectiveClause = do
  c <- symbol
  delimiter "=>"
  body <- rawExpr
  return (c, body)

getIntrospectiveValue :: Hint -> T.Text -> App T.Text
getIntrospectiveValue m key = do
  tp <- Env.getTargetPlatform
  case key of
    "target-platform" -> do
      return $ TP.reify tp
    "target-arch" ->
      return $ Arch.reify (TP.arch tp)
    "target-os" ->
      return $ OS.reify (TP.os tp)
    _ ->
      Throw.raiseError m $ "no such introspective value is defined: " <> key

rawTermPiElimOrSymbol :: Parser RT.RawTerm
rawTermPiElimOrSymbol = do
  m <- getCurrentHint
  e <- rawTermParseSymbol
  funcVarList <- many $ do
    delimiter "::"
    rawTermParseSymbol
  return $ foldReversePiElim m e funcVarList

rawTermParseSymbol :: Parser RT.RawTerm
rawTermParseSymbol = do
  (m, varOrLocator) <- parseName
  return $ m :< RT.Var varOrLocator

foldReversePiElim :: Hint -> RT.RawTerm -> [RT.RawTerm] -> RT.RawTerm
foldReversePiElim m e funcVarList =
  case funcVarList of
    [] ->
      e
    func : rest ->
      foldReversePiElim m (m :< RT.PiElim func [e]) rest

rawTermTextIntro :: Parser RT.RawTerm
rawTermTextIntro = do
  m <- getCurrentHint
  s <- string
  textType <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreText
  return $ m :< RT.Prim (WP.Value (WPV.StaticText textType s))

rawTermInteger :: Parser RT.RawTerm
rawTermInteger = do
  m <- getCurrentHint
  intValue <- try integer
  h <- lift $ Gensym.newPreHole m
  return $ m :< RT.Prim (WP.Value (WPV.Int h intValue))

rawTermFloat :: Parser RT.RawTerm
rawTermFloat = do
  m <- getCurrentHint
  floatValue <- try float
  h <- lift $ Gensym.newPreHole m
  return $ m :< RT.Prim (WP.Value (WPV.Float h floatValue))

lam :: Hint -> [RawBinder RT.RawTerm] -> RT.RawTerm -> RT.RawTerm
lam m varList e =
  m :< RT.PiIntro (LK.Normal O.Transparent) varList e

preVar :: Hint -> T.Text -> RT.RawTerm
preVar m str =
  m :< RT.Var (Var str)

handleDefiniteDescriptionIntoRawConsName :: Hint -> T.Text -> App Name
handleDefiniteDescriptionIntoRawConsName m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair m text
  return $ Locator (gl, ll)

handleDefiniteDescriptionIntoVarGlobal :: Hint -> T.Text -> App RT.RawTerm
handleDefiniteDescriptionIntoVarGlobal m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair m text
  return $ m :< RT.Var (Locator (gl, ll))
