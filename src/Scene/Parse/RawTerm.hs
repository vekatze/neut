module Scene.Parse.RawTerm
  ( rawExpr,
    preAscription,
    preBinder,
    parseTopDefInfo,
    parseDeclareItem,
    parseDefInfoCod,
    typeWithoutIdent,
    preVar,
    parseName,
    lowType,
  )
where

import Context.App
import Context.Decl qualified as Decl
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Trans
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.IsExplicit
import Entity.Key
import Entity.LowType qualified as LT
import Entity.Magic qualified as M
import Entity.Name
import Entity.PrimType qualified as PT
import Entity.PrimType.FromText qualified as PT
import Entity.RawBinder
import Entity.RawDecl qualified as RDE
import Entity.RawIdent
import Entity.RawLamKind qualified as LK
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Scene.Parse.Core
import Text.Megaparsec

rawExpr :: Parser RT.RawTerm
rawExpr = do
  m <- getCurrentHint
  choice
    [ do
        thunk <- rawExprLet m
        thunk <$> rawExpr,
      do
        termOrSeq <- rawExprSeqOrTerm m
        case termOrSeq of
          Left term ->
            return term
          Right k ->
            k <$> rawExpr
    ]

rawExprLet :: Hint -> Parser (RT.RawTerm -> RT.RawTerm)
rawExprLet m = do
  choice
    [ rawTermLet m,
      rawTermUse m
    ]

rawExprSeqOrTerm :: Hint -> Parser (Either RT.RawTerm (RT.RawTerm -> RT.RawTerm))
rawExprSeqOrTerm m = do
  e1 <- rawTerm
  choice
    [ do
        delimiter ";"
        return $ Right $ \e2 -> m :< RT.Seq e1 e2,
      return $ Left e1
    ]

rawTerm :: Parser RT.RawTerm
rawTerm = do
  choice
    [ try rawTermPiGeneral,
      try rawTermPiIntro,
      rawTermPiOrConsOrAscOrBasic
    ]

rawTermBasic :: Parser RT.RawTerm
rawTermBasic = do
  choice
    [ rawTermDefine,
      rawTermPiElimExact,
      rawTermPiElimExplicit,
      rawTermIntrospect,
      rawTermMagic,
      rawTermMatch,
      rawTermIf,
      rawTermWhen,
      rawTermAssert,
      rawTermNoema,
      rawTermFlowIntro,
      rawTermFlowElim,
      rawTermOption,
      rawTermEmbody,
      rawTermTuple,
      rawTermTupleIntro,
      rawTermWith,
      rawTermIdealize,
      rawTermPiElimOrSimple
    ]

{-# INLINE rawTermSimple #-}
rawTermSimple :: Parser RT.RawTerm
rawTermSimple = do
  choice
    [ rawTermBrace,
      rawTermListIntro,
      rawTermTextIntro,
      rawTermTau,
      rawTermAdmit,
      rawTermHole,
      rawTermInteger,
      rawTermFloat,
      rawTermSymbol
    ]

rawTermPiGeneral :: Parser RT.RawTerm
rawTermPiGeneral = do
  m <- getCurrentHint
  impArgs <- parseImplicitArgs
  expArgs <- argList $ choice [try preAscription, typeWithoutIdent]
  delimiter "->"
  cod <- rawTerm
  return $ m :< RT.Pi impArgs expArgs cod

rawTermPiIntro :: Parser RT.RawTerm
rawTermPiIntro = do
  m <- getCurrentHint
  impArgs <- parseImplicitArgs
  expArgs <- argList preBinder
  delimiter "=>"
  e <- rawExpr
  return $ m :< RT.PiIntro LK.Normal impArgs expArgs e

rawTermPiOrConsOrAscOrBasic :: Parser RT.RawTerm
rawTermPiOrConsOrAscOrBasic = do
  m <- getCurrentHint
  basic <- rawTermBasic
  choice
    [ do
        delimiter "->"
        x <- lift Gensym.newTextForHole
        cod <- rawTerm
        return $ m :< RT.Pi [] [(m, x, basic)] cod,
      do
        delimiter "::"
        rest <- rawTerm
        listCons <- lift $ locatorToVarGlobal m coreListCons
        return $ m :< RT.piElim listCons [basic, rest],
      do
        delimiter ":"
        t <- rawTerm
        ascribe m t basic,
      return basic
    ]

rawTermKeyValuePair :: Parser (Hint, Key, RT.RawTerm)
rawTermKeyValuePair = do
  (m, key) <- var
  choice
    [ do
        delimiter "="
        value <- rawExpr
        return (m, key, value),
      do
        return (m, key, m :< RT.Var (Var key))
    ]

rawTermLet :: Hint -> Parser (RT.RawTerm -> RT.RawTerm)
rawTermLet mLet = do
  letKind <-
    choice
      [ keyword "let" >> return RT.Plain,
        keyword "try" >> return RT.Try,
        keyword "bind" >> return RT.Bind,
        keyword "tie" >> return RT.Noetic
      ]
  (mx, patInner) <- rawTermPattern
  t <- rawTermLetVarAscription mx
  noeticVarList <-
    choice
      [ keyword "on" >> commaList rawTermNoeticVar,
        return []
      ]
  lift $ ensureIdentLinearity S.empty noeticVarList
  let mxt = (mx, patInner, t)
  delimiter "="
  e1 <- rawExpr
  delimiter "in"
  return $ \e2 -> mLet :< RT.Let letKind mxt noeticVarList e1 e2

rawTermUse :: Hint -> Parser (RT.RawTerm -> RT.RawTerm)
rawTermUse m = do
  keyword "use"
  e <- rawTerm
  xs <- betweenBrace $ commaList preBinder
  delimiter "in"
  lift $ ensureIdentLinearity S.empty $ map (\(mx, x, _) -> (mx, x)) xs
  return $ \cont -> m :< RT.Use e xs cont

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
  tmp <- lift Gensym.newTextForHole
  return $ bind (m, tmp, t) e (rawVar m (Var tmp))

rawTermLetVarAscription' :: Parser (Maybe RT.RawTerm)
rawTermLetVarAscription' =
  choice
    [ try $ do
        delimiter ":"
        Just <$> rawTerm,
      return Nothing
    ]

ensureIdentLinearity :: S.Set RawIdent -> [(Hint, RawIdent)] -> App ()
ensureIdentLinearity foundVarSet vs =
  case vs of
    [] ->
      return ()
    (m, name) : rest
      | S.member name foundVarSet ->
          Throw.raiseError m $ "found a non-linear occurrence of `" <> name <> "`."
      | otherwise ->
          ensureIdentLinearity (S.insert name foundVarSet) rest

rawTermNoeticVar :: Parser (Hint, T.Text)
rawTermNoeticVar = do
  (m, x) <- var
  return (m, x)

rawTermIdealize :: Parser RT.RawTerm
rawTermIdealize = do
  m <- getCurrentHint
  keyword "idealize"
  xs <- commaList var
  cont <- betweenBrace rawExpr
  return $ m :< RT.Idealize xs cont

rawTermEmbody :: Parser RT.RawTerm
rawTermEmbody = do
  m <- getCurrentHint
  delimiter "*"
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

parseDefInfo :: Hint -> Parser RT.DefInfo
parseDefInfo m = do
  functionVar <- var
  impArgs <- parseImplicitArgs
  expArgs <- argList preBinder
  codType <- parseDefInfoCod m
  e <- betweenBrace rawExpr
  return (functionVar, impArgs, expArgs, codType, e)

parseTopDefInfo :: Parser RT.TopDefInfo
parseTopDefInfo = do
  topDefHeader <- parseTopDefHeader
  e <- betweenBrace rawExpr
  return (topDefHeader, e)

parseTopDefHeader :: Parser RT.TopDefHeader
parseTopDefHeader = do
  m <- getCurrentHint
  funcBaseName <- baseName
  impDomArgList <- parseImplicitArgs
  expDomArgList <- argSeqOrList preBinder
  lift $ ensureArgumentLinearity S.empty $ map (\(mx, x, _) -> (mx, x)) expDomArgList
  codType <- parseDefInfoCod m
  return ((m, funcBaseName), impDomArgList, expDomArgList, codType)

parseDeclareItem :: (BN.BaseName -> App DD.DefiniteDescription) -> Parser RDE.RawDecl
parseDeclareItem nameLifter = do
  loc <- getCurrentHint
  name <- baseName >>= lift . nameLifter
  (isConstLike, impArgs, expArgs) <-
    choice
      [ do
          impArgs <- parseImplicitArgs
          choice
            [ do
                expDomArgList <- argSeqOrList preBinder
                return (False, impArgs, expDomArgList),
              return (True, impArgs, [])
            ],
        do
          return (True, [], [])
      ]
  delimiter ":"
  cod <- rawTerm
  return RDE.RawDecl {loc, name, isConstLike, impArgs, expArgs, cod}

parseImplicitArgs :: Parser [RawBinder RT.RawTerm]
parseImplicitArgs =
  choice
    [ parseImplicitArgs',
      return []
    ]

parseImplicitArgs' :: Parser [RawBinder RT.RawTerm]
parseImplicitArgs' =
  betweenAngle (commaList preBinder)

ensureArgumentLinearity :: S.Set RawIdent -> [(Hint, RawIdent)] -> App ()
ensureArgumentLinearity foundVarSet vs =
  case vs of
    [] ->
      return ()
    (m, name) : rest
      | S.member name foundVarSet ->
          Throw.raiseError m $ "found a non-linear occurrence of `" <> name <> "`."
      | otherwise ->
          ensureArgumentLinearity (S.insert name foundVarSet) rest

parseDefInfoCod :: Hint -> Parser RT.RawTerm
parseDefInfoCod m =
  choice
    [ do
        delimiter ":"
        rawTerm,
      lift $ Gensym.newPreHole m
    ]

rawTermDefine :: Parser RT.RawTerm
rawTermDefine = do
  m <- getCurrentHint
  keyword "define"
  ((mFun, functionName), impArgs, expArgs, codType, e) <- parseDefInfo m
  return $ m :< RT.PiIntro (LK.Fix (mFun, functionName, codType)) impArgs expArgs e

rawTermMagic :: Parser RT.RawTerm
rawTermMagic = do
  m <- getCurrentHint
  keyword "magic"
  choice
    [ rawTermMagicCast m,
      rawTermMagicStore m,
      rawTermMagicLoad m,
      rawTermMagicExternal m,
      rawTermMagicGlobal m
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
    value <- delimiter "," >> rawTerm
    pointer <- delimiter "," >> rawTerm
    return $ m :< RT.Magic (M.Store lt value pointer)

rawTermMagicLoad :: Hint -> Parser RT.RawTerm
rawTermMagicLoad m = do
  rawTermMagicBase "load" $ do
    lt <- lowType
    pointer <- delimiter "," >> rawTerm
    return $ m :< RT.Magic (M.Load lt pointer)

rawTermMagicExternal :: Hint -> Parser RT.RawTerm
rawTermMagicExternal m = do
  rawTermMagicBase "external" $ do
    extFunName <- EN.ExternalName <$> symbol
    es <- many (delimiter "," >> rawTerm)
    varArgAndTypeList <-
      choice
        [ do
            delimiter ";"
            sepBy rawTermAndLowType (delimiter ","),
          return
            []
        ]
    (domList, cod) <- lift $ Decl.lookupDeclEnv m (DN.Ext extFunName)
    return $ m :< RT.Magic (M.External domList cod extFunName es varArgAndTypeList)

rawTermAndLowType :: Parser (LT.LowType, RT.RawTerm)
rawTermAndLowType = do
  e <- rawTerm
  t <- lowType
  return (t, e)

rawTermMagicGlobal :: Hint -> Parser RT.RawTerm
rawTermMagicGlobal m = do
  rawTermMagicBase "global" $ do
    globalVarName <- string
    delimiter ","
    lt <- lowType
    return $ m :< RT.Magic (M.Global lt (EN.ExternalName globalVarName))

lowType :: Parser LT.LowType
lowType = do
  choice
    [ lowTypePointer,
      lowTypeVoid,
      lowTypeArray,
      lowTypeStruct,
      lowTypeNumber
    ]

lowTypePointer :: Parser LT.LowType
lowTypePointer = do
  keyword "pointer"
  return LT.Pointer

lowTypeVoid :: Parser LT.LowType
lowTypeVoid = do
  keyword "void"
  return LT.Void

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
  isNoetic <- choice [try (keyword "case") >> return True, keyword "match" >> return False]
  es <- commaList rawTermBasic
  patternRowList <- betweenBrace $ manyList $ rawTermPatternRow (length es)
  return $ m :< RT.DataElim isNoetic es (RP.new patternRowList)

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
        delimiter "::"
        pat <- rawTermPattern
        listCons <- lift $ locatorToName m coreListCons
        return (m, RP.Cons listCons (RP.Paren [headPat, pat])),
      return headPat
    ]

rawTermPatternBasic :: Parser (Hint, RP.RawPattern)
rawTermPatternBasic =
  choice
    [ rawTermPatternListIntro,
      try rawTermPatternTupleIntro,
      rawTermPatternConsOrVar
    ]

rawTermPatternListIntro :: Parser (Hint, RP.RawPattern)
rawTermPatternListIntro = do
  m <- getCurrentHint
  patList <- betweenBracket $ commaList rawTermPattern
  return (m, RP.ListIntro patList)

rawTermPatternTupleIntro :: Parser (Hint, RP.RawPattern)
rawTermPatternTupleIntro = do
  m <- getCurrentHint
  keyword "Tuple"
  patList <- betweenParen $ commaList rawTermPattern
  unitVar <- lift $ locatorToName m coreUnitUnit
  pairVar <- lift $ locatorToName m corePairPair
  return $ foldTuplePat m unitVar pairVar patList

foldTuplePat :: Hint -> Name -> Name -> [(Hint, RP.RawPattern)] -> (Hint, RP.RawPattern)
foldTuplePat m unitVar pairVar es =
  case es of
    [] ->
      (blur m, RP.Var unitVar)
    [e] ->
      e
    e : rest -> do
      let rest' = foldTuplePat m unitVar pairVar rest
      (blur m, RP.Cons pairVar (RP.Paren [e, rest']))

parseName :: Parser (Hint, Name)
parseName = do
  (m, varText) <- var
  interpretVarName m varText

rawTermPatternConsOrVar :: Parser (Hint, RP.RawPattern)
rawTermPatternConsOrVar = do
  (m, varOrLocator) <- parseName
  choice
    [ do
        patArgs <- argList rawTermPattern
        return (m, RP.Cons varOrLocator (RP.Paren patArgs)),
      do
        keyword "of"
        kvs <- betweenBrace $ bulletListOrCommaSeq rawTermPatternKeyValuePair
        return (m, RP.Cons varOrLocator (RP.Of kvs)),
      do
        return (m, RP.Var varOrLocator)
    ]

rawTermPatternKeyValuePair :: Parser (Key, (Hint, RP.RawPattern))
rawTermPatternKeyValuePair = do
  mFrom <- getCurrentHint
  from <- symbol
  choice
    [ do
        delimiter "="
        to <- rawTermPattern
        return (from, to),
      do
        return (from, (mFrom, RP.Var (Var from))) -- record rhyming
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
  return $ m :< RT.If ifCond ifBody elseIfList elseBody

rawTermWhen :: Parser RT.RawTerm
rawTermWhen = do
  m <- getCurrentHint
  keyword "when"
  whenCond <- rawTerm
  whenBody <- betweenBrace rawExpr
  return $ m :< RT.When whenCond whenBody

rawTermBrace :: Parser RT.RawTerm
rawTermBrace =
  betweenBrace rawExpr

rawTermTuple :: Parser RT.RawTerm
rawTermTuple = do
  m <- getCurrentHint
  keyword "tuple"
  ts <- betweenParen $ commaList rawExpr
  return $ m :< RT.Tuple ts

rawTermTupleIntro :: Parser RT.RawTerm
rawTermTupleIntro = do
  m <- getCurrentHint
  keyword "Tuple"
  es <- betweenParen $ commaList rawExpr
  return $ m :< RT.TupleIntro es

rawTermWith :: Parser RT.RawTerm
rawTermWith = do
  m <- getCurrentHint
  keyword "with"
  binder <- rawTerm
  body <- betweenBrace rawExpr
  return $ m :< RT.With binder body

bind :: RawBinder RT.RawTerm -> RT.RawTerm -> RT.RawTerm -> RT.RawTerm
bind (m, x, t) e cont =
  m :< RT.Let RT.Plain (m, RP.Var (Var x), t) [] e cont

rawTermNoema :: Parser RT.RawTerm
rawTermNoema = do
  m <- getCurrentHint
  delimiter "&"
  t <- rawTermBasic
  return $ m :< RT.Noema t

rawTermFlowIntro :: Parser RT.RawTerm
rawTermFlowIntro = do
  m <- getCurrentHint
  keyword "detach"
  e <- betweenBrace rawExpr
  return $ m :< RT.Detach e

rawTermFlowElim :: Parser RT.RawTerm
rawTermFlowElim = do
  m <- getCurrentHint
  keyword "attach"
  e <- betweenBrace rawExpr
  return $ m :< RT.Attach e

rawTermOption :: Parser RT.RawTerm
rawTermOption = do
  m <- getCurrentHint
  delimiter "?"
  t <- rawTermBasic
  return $ m :< RT.Option t

rawTermAdmit :: Parser RT.RawTerm
rawTermAdmit = do
  m <- getCurrentHint
  keyword "admit"
  return $ m :< RT.Admit

rawTermAssert :: Parser RT.RawTerm
rawTermAssert = do
  m <- getCurrentHint
  keyword "assert"
  mText <- getCurrentHint
  message <- string
  e <- betweenBrace rawExpr
  return $ m :< RT.Assert (mText, message) e

rawTermPiElimOrSimple :: Parser RT.RawTerm
rawTermPiElimOrSimple = do
  m <- getCurrentHint
  e <- rawTermSimple
  case e of
    _ :< RT.Var name -> do
      choice
        [ do
            keyword "of"
            rowList <- betweenBrace $ bulletListOrCommaSeq rawTermKeyValuePair
            return $ m :< RT.PiElimByKey False name rowList,
          rawTermPiElimCont False m e
        ]
    _ -> do
      rawTermPiElimCont False m e

rawTermPiElimCont :: IsExplicit -> Hint -> RT.RawTerm -> Parser RT.RawTerm
rawTermPiElimCont isExplicit m e = do
  argListList <- many $ argList rawExpr
  foldPiElim isExplicit m e argListList

foldPiElim :: IsExplicit -> Hint -> RT.RawTerm -> [[RT.RawTerm]] -> Parser RT.RawTerm
foldPiElim isExplicit m e argListList =
  case argListList of
    [] ->
      return e
    args : rest ->
      foldPiElim isExplicit m (m :< RT.PiElim isExplicit e args) rest

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
  es <- betweenBracket $ commaList rawExpr
  return $ m :< RT.ListIntro es

rawTermPiElimExact :: Parser RT.RawTerm
rawTermPiElimExact = do
  m <- getCurrentHint
  keyword "exact"
  e <- rawTerm
  return $ m :< RT.PiElimExact e

rawTermPiElimExplicit :: Parser RT.RawTerm
rawTermPiElimExplicit = do
  m <- getCurrentHint
  keyword "call"
  e <- rawTermSimple
  case e of
    _ :< RT.Var name -> do
      choice
        [ do
            keyword "of"
            rowList <- betweenBrace $ bulletListOrCommaSeq rawTermKeyValuePair
            return $ m :< RT.PiElimByKey True name rowList,
          rawTermPiElimCont True m e
        ]
    _ -> do
      rawTermPiElimCont True m e

rawTermIntrospect :: Parser RT.RawTerm
rawTermIntrospect = do
  m <- getCurrentHint
  keyword "introspect"
  key <- symbol
  clauseList <- betweenBrace $ manyList rawTermIntrospectiveClause
  return $ m :< RT.Introspect key clauseList

rawTermIntrospectiveClause :: Parser (Maybe T.Text, RT.RawTerm)
rawTermIntrospectiveClause = do
  c <- symbol
  delimiter "=>"
  body <- rawExpr
  if c /= "default"
    then return (Just c, body)
    else return (Nothing, body)

rawTermSymbol :: Parser RT.RawTerm
rawTermSymbol = do
  (m, varOrLocator) <- parseVarName
  return $ m :< RT.Var varOrLocator

parseVarName :: Parser (Hint, Name)
parseVarName = do
  (m, varText) <- var
  interpretVarName m varText

interpretVarName :: Hint -> T.Text -> Parser (Hint, Name)
interpretVarName m varText = do
  case DD.getLocatorPair m varText of
    Left _ ->
      return (m, Var varText)
    Right (gl, ll) ->
      return (m, Locator (gl, ll))

rawTermTextIntro :: Parser RT.RawTerm
rawTermTextIntro = do
  m <- getCurrentHint
  s <- string
  textType <- lift $ locatorToVarGlobal m coreText
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

preVar :: Hint -> T.Text -> RT.RawTerm
preVar m str =
  rawVar m (Var str)

locatorToName :: Hint -> T.Text -> App Name
locatorToName m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair m text
  return $ Locator (gl, ll)

locatorToVarGlobal :: Hint -> T.Text -> App RT.RawTerm
locatorToVarGlobal m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair (blur m) text
  return $ rawVar (blur m) (Locator (gl, ll))

rawVar :: Hint -> Name -> RT.RawTerm
rawVar m name =
  m :< RT.Var name
