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
import Entity.Annotation qualified as Annot
import Entity.Arch qualified as Arch
import Entity.BaseName qualified as BN
import Entity.BuildMode qualified as BM
import Entity.Const
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.Hint.Reify
import Entity.IsExplicit
import Entity.Key
import Entity.Locator qualified as L
import Entity.LowType qualified as LT
import Entity.Magic qualified as M
import Entity.Name
import Entity.Noema qualified as N
import Entity.OS qualified as OS
import Entity.Platform qualified as Platform
import Entity.PrimType qualified as PT
import Entity.PrimType.FromText qualified as PT
import Entity.RawBinder
import Entity.RawDecl qualified as RDE
import Entity.RawIdent
import Entity.RawLamKind qualified as LK
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.Remark
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
    [ try rawTermLetExcept,
      rawTermLetOrLetOn m,
      rawTermUse m
    ]

rawExprSeqOrTerm :: Hint -> Parser (Either RT.RawTerm (RT.RawTerm -> RT.RawTerm))
rawExprSeqOrTerm m = do
  e1 <- rawTerm
  choice
    [ do
        delimiter ";"
        f <- lift Gensym.newTextForHole
        unit <- lift $ locatorToVarGlobal m coreUnit
        return $ Right $ \e2 -> bind (m, f, unit) e1 e2,
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

rawTermLetOrLetOn :: Hint -> Parser (RT.RawTerm -> RT.RawTerm)
rawTermLetOrLetOn mLet = do
  isNoetic <- choice [try (keyword "tie") >> return True, keyword "let" >> return False]
  pat@(mx, _) <- rawTermPattern
  (x, modifier) <- getContinuationModifier pat
  t <- rawTermLetVarAscription mx
  let mxt = (mx, x, t)
  choice
    [ do
        keyword "on"
        noeticVarList <- commaList rawTermNoeticVar
        lift $ ensureIdentLinearity S.empty noeticVarList
        delimiter "="
        e1 <- rawExpr
        delimiter "in"
        return $ \e2 -> mLet :< RT.Let mxt noeticVarList e1 (modifier isNoetic e2),
      do
        delimiter "="
        e1 <- rawExpr
        delimiter "in"
        return $ \e2 -> mLet :< RT.Let mxt [] e1 (modifier isNoetic e2)
    ]

rawTermUse :: Hint -> Parser (RT.RawTerm -> RT.RawTerm)
rawTermUse m = do
  keyword "use"
  e <- rawTerm
  xs <- betweenBrace $ commaList preBinder
  delimiter "in"
  lift $ ensureIdentLinearity S.empty $ map (\(mx, x, _) -> (mx, x)) xs
  return $ \cont -> m :< RT.Use e xs cont

getContinuationModifier :: (Hint, RP.RawPattern) -> Parser (RawIdent, N.IsNoetic -> RT.RawTerm -> RT.RawTerm)
getContinuationModifier pat =
  case pat of
    (_, RP.Var (Var x))
      | not (isConsName x) ->
          return (x, \_ cont -> cont)
    _ -> do
      tmp <- lift Gensym.newTextForHole
      return
        ( tmp,
          \isNoetic cont@(mCont :< _) ->
            mCont :< RT.DataElim isNoetic [rawVar mCont (Var tmp)] (RP.new [(V.fromList [pat], cont)])
        )

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

rawTermLetExcept :: Parser (RT.RawTerm -> RT.RawTerm)
rawTermLetExcept = do
  keyword "try"
  pat@(mx, _) <- rawTermPattern
  rightType <- rawTermLetVarAscription mx
  delimiter "="
  e1@(m1 :< _) <- rawExpr
  delimiter "in"
  eitherTypeInner <- lift $ locatorToVarGlobal mx coreExcept
  leftType <- lift $ Gensym.newPreHole m1
  let eitherType = m1 :< RT.piElim eitherTypeInner [leftType, rightType]
  e1' <- ascribe m1 eitherType e1
  m2 <- getCurrentHint
  err <- lift Gensym.newText
  exceptFail <- lift $ locatorToName m2 coreExceptFail
  exceptPass <- lift $ locatorToName m2 coreExceptPass
  exceptFailVar <- lift $ locatorToVarGlobal mx coreExceptFail
  let _m = blur m2
  return $ \e2 ->
    m2
      :< RT.DataElim
        False
        [e1']
        ( RP.new
            [ ( V.fromList [(_m, RP.Cons exceptFail (RP.Paren [(_m, RP.Var (Var err))]))],
                m2 :< RT.piElim exceptFailVar [preVar m2 err]
              ),
              (V.fromList [(_m, RP.Cons exceptPass (RP.Paren [pat]))], e2)
            ]
        )

rawTermIdealize :: Parser RT.RawTerm
rawTermIdealize = do
  m <- getCurrentHint
  keyword "idealize"
  xs <- commaList var
  cont <- betweenBrace rawExpr
  result <- lift Gensym.newTextForHole
  t <- lift $ Gensym.newPreHole m
  holes <- forM xs $ \(mx, _) -> lift $ do
    hole <- Gensym.newTextForHole
    tHole <- Gensym.newPreHole mx
    return (mx, hole, tHole)
  let resultVar = rawVar m (Var result)
  let retResult = foldr (\(binder, (mx, x)) acc -> bind binder (rawVar mx (Var x)) acc) resultVar (zip holes xs)
  return $ m :< RT.Let (m, result, t) xs cont retResult

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

foldByOp :: Hint -> Name -> [RT.RawTerm] -> RT.RawTerm
foldByOp m op es =
  case es of
    [] ->
      error "RawTerm.foldByOp: invalid argument"
    [e] ->
      e
    e : rest ->
      m :< RT.piElim (rawVar (blur m) op) [e, foldByOp m op rest]

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
  listNil <- lift $ Throw.liftEither $ DD.getLocatorPair m coreListNil
  listCons <- lift $ locatorToName m coreListCons
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
      (m, RP.Cons listCons (RP.Paren [e, rest']))

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
  boolTrue <- lift $ locatorToName (blur m) coreBoolTrue
  boolFalse <- lift $ locatorToName (blur m) coreBoolFalse
  unitUnit <- lift $ locatorToVarGlobal m coreUnitUnit
  return $ foldIf m boolTrue boolFalse whenCond whenBody [] unitUnit

foldIf ::
  Hint ->
  Name ->
  Name ->
  RT.RawTerm ->
  RT.RawTerm ->
  [(RT.RawTerm, RT.RawTerm)] ->
  RT.RawTerm ->
  RT.RawTerm
foldIf m true false ifCond ifBody elseIfList elseBody =
  case elseIfList of
    [] -> do
      m
        :< RT.DataElim
          False
          [ifCond]
          ( RP.new
              [ (V.fromList [(blur m, RP.Var true)], ifBody),
                (V.fromList [(blur m, RP.Var false)], elseBody)
              ]
          )
    ((elseIfCond, elseIfBody) : rest) -> do
      let cont = foldIf m true false elseIfCond elseIfBody rest elseBody
      m
        :< RT.DataElim
          False
          [ifCond]
          ( RP.new
              [ (V.fromList [(blur m, RP.Var true)], ifBody),
                (V.fromList [(blur m, RP.Var false)], cont)
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
  unitVar <- lift $ locatorToName m coreUnit
  pairVar <- lift $ locatorToName m corePair
  case es of
    [] ->
      return $ rawVar (blur m) unitVar
    [e] ->
      return e
    _ ->
      return $ foldByOp m pairVar es

rawTermTupleIntro :: Parser RT.RawTerm
rawTermTupleIntro = do
  m <- getCurrentHint
  keyword "Tuple"
  es <- betweenParen $ commaList rawExpr
  unitVar <- lift $ locatorToName m coreUnitUnit
  pairVar <- lift $ locatorToName m corePairPair
  case es of
    [] ->
      return $ rawVar m unitVar
    [e] ->
      return e
    _ ->
      return $ foldByOp m pairVar es

rawTermWith :: Parser RT.RawTerm
rawTermWith = do
  m <- getCurrentHint
  keyword "with"
  binder <- rawTerm
  betweenBrace $ rawTermWith' m binder

rawTermWith' :: Hint -> RT.RawTerm -> Parser RT.RawTerm
rawTermWith' m binder = do
  choice
    [ do
        thunk <- rawExprLet m
        thunk <$> rawTermWith' m binder,
      do
        thunk <- rawExprBind binder
        thunk <$> rawTermWith' m binder,
      do
        termOrThunk <- rawExprSeqOrTerm m
        case termOrThunk of
          Left term ->
            return term
          Right thunk -> do
            thunk <$> rawTermWith' m binder
    ]

rawExprBind :: RT.RawTerm -> Parser (RT.RawTerm -> RT.RawTerm)
rawExprBind binder = do
  m <- getCurrentHint
  keyword "bind"
  pat@(mx, _) <- rawTermPattern
  (x, modifier) <- getContinuationModifier pat
  t <- rawTermLetVarAscription mx
  let mxt = (mx, x, t)
  delimiter "="
  -- e1 <- rawExpr
  e1 <- rawTermWith' m binder
  delimiter "in"
  return $ \e2 -> m :< RT.piElim binder [e1, lam m [mxt] (modifier False e2)]

bind :: RawBinder RT.RawTerm -> RT.RawTerm -> RT.RawTerm -> RT.RawTerm
bind mxt@(m, _, _) e cont =
  m :< RT.Let mxt [] e cont

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
  t <- lift $ Gensym.newPreHole (blur m)
  detachVar <- lift $ locatorToVarGlobal m coreThreadDetach
  e <- rawTermSimple
  return $ m :< RT.piElim detachVar [t, lam m [] e]

rawTermFlowElim :: Parser RT.RawTerm
rawTermFlowElim = do
  m <- getCurrentHint
  keyword "attach"
  t <- lift $ Gensym.newPreHole (blur m)
  attachVar <- lift $ locatorToVarGlobal m coreThreadAttach
  e <- rawTermSimple
  return $ m :< RT.piElim attachVar [t, e]

rawTermOption :: Parser RT.RawTerm
rawTermOption = do
  m <- getCurrentHint
  delimiter "?"
  t <- rawTermBasic
  exceptVar <- lift $ locatorToVarGlobal m coreExcept
  unit <- lift $ locatorToVarGlobal m coreUnit
  return $ m :< RT.piElim exceptVar [unit, t]

rawTermAdmit :: Parser RT.RawTerm
rawTermAdmit = do
  m <- getCurrentHint
  keyword "admit"
  admit <- lift $ locatorToVarGlobal m coreSystemAdmit
  t <- lift $ Gensym.newPreHole (blur m)
  textType <- lift $ locatorToVarGlobal m coreText
  return $
    m
      :< RT.Annotation
        Warning
        (Annot.Type ())
        ( m
            :< RT.piElim
              admit
              [t, m :< RT.Prim (WP.Value (WPV.StaticText textType ("admit: " <> T.pack (toString m) <> "\n")))]
        )

rawTermAssert :: Parser RT.RawTerm
rawTermAssert = do
  m <- getCurrentHint
  keyword "assert"
  mText <- getCurrentHint
  message <- string
  e@(mCond :< _) <- betweenBrace rawExpr
  assert <- lift $ locatorToVarGlobal m coreSystemAssert
  textType <- lift $ locatorToVarGlobal m coreText
  let fullMessage = T.pack (toString m) <> "\nassertion failure: " <> message <> "\n"
  return $
    m
      :< RT.piElim
        assert
        [mText :< RT.Prim (WP.Value (WPV.StaticText textType fullMessage)), lam mCond [] e]

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
  es <- betweenBracket $ commaList rawTerm
  listNil <- lift $ locatorToVarGlobal m coreListNil
  listCons <- lift $ locatorToVarGlobal m coreListCons
  return $ foldListApp m listNil listCons es

foldListApp :: Hint -> RT.RawTerm -> RT.RawTerm -> [RT.RawTerm] -> RT.RawTerm
foldListApp m listNil listCons es =
  case es of
    [] ->
      listNil
    e : rest ->
      m :< RT.piElim listCons [e, foldListApp m listNil listCons rest]

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
  value <- lift $ getIntrospectiveValue m key
  clauseList <- betweenBrace $ manyList rawTermIntrospectiveClause
  lift $ lookupIntrospectiveClause m value clauseList

lookupIntrospectiveClause :: Hint -> T.Text -> [(Maybe T.Text, RT.RawTerm)] -> App RT.RawTerm
lookupIntrospectiveClause m value clauseList =
  case clauseList of
    [] ->
      Throw.raiseError m $ "this term doesn't support `" <> value <> "`."
    (Just key, clause) : rest
      | key == value ->
          return clause
      | otherwise ->
          lookupIntrospectiveClause m value rest
    (Nothing, clause) : _ ->
      return clause

rawTermIntrospectiveClause :: Parser (Maybe T.Text, RT.RawTerm)
rawTermIntrospectiveClause = do
  c <- symbol
  delimiter "=>"
  body <- rawExpr
  if c /= "default"
    then return (Just c, body)
    else return (Nothing, body)

getIntrospectiveValue :: Hint -> T.Text -> App T.Text
getIntrospectiveValue m key = do
  bm <- Env.getBuildMode
  case key of
    "platform" -> do
      return $ Platform.reify Platform.platform
    "arch" ->
      return $ Arch.reify (Platform.arch Platform.platform)
    "os" ->
      return $ OS.reify (Platform.os Platform.platform)
    "build-mode" ->
      return $ BM.reify bm
    _ ->
      Throw.raiseError m $ "no such introspective value is defined: " <> key

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

lam :: Hint -> [RawBinder RT.RawTerm] -> RT.RawTerm -> RT.RawTerm
lam m varList e =
  m :< RT.PiIntro LK.Normal [] varList e

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
