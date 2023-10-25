module Scene.Parse.RawTerm
  ( rawExpr,
    preAscription,
    preBinder,
    parseTopDefInfo,
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
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.Annotation qualified as Annot
import Entity.Arch qualified as Arch
import Entity.Attr.Var (Attr (isExplicit))
import Entity.Attr.Var qualified as AttrV
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
      rawTermLetOrLetOn m
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
      rawTermPiIntro,
      rawTermPiOrConsOrAscOrBasic
    ]

rawTermBasic :: Parser RT.RawTerm
rawTermBasic = do
  choice
    [ rawTermMu,
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
      rawTermOptionNone,
      rawTermOptionSome,
      rawTermEmbody,
      rawTermTuple,
      rawTermTupleIntro,
      rawTermWith,
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
  domList <- argList $ choice [try preAscription, typeWithoutIdent]
  delimiter "->"
  cod <- rawTerm
  return $ m :< RT.Pi domList cod

rawTermPiIntro :: Parser RT.RawTerm
rawTermPiIntro = do
  m <- getCurrentHint
  varList <- argList preBinder
  delimiter "=>"
  lam m varList <$> betweenBrace rawExpr

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
        delimiter "::"
        rest <- rawTerm
        listCons <- lift $ locatorToVarGlobal m coreListCons
        return $ m :< RT.PiElim listCons [basic, rest],
      do
        delimiter ":"
        t <- rawTerm
        ascribe m t basic,
      return basic
    ]

rawTermKeyValuePair :: Parser (Hint, Key, RT.RawTerm)
rawTermKeyValuePair = do
  (m, key) <- var
  delimiter "=>"
  value <- rawExpr
  return (m, key, value)

rawTermLetOrLetOn :: Hint -> Parser (RT.RawTerm -> RT.RawTerm)
rawTermLetOrLetOn m = do
  isNoetic <- choice [try (keyword "&let") >> return True, keyword "let" >> return False]
  pat@(mx, _) <- rawTermPattern
  (x, modifier) <- getContinuationModifier pat
  t <- rawTermLetVarAscription mx
  let mxt = (mx, x, t)
  choice
    [ do
        keyword "on"
        noeticVarList <- commaList rawTermNoeticVar
        lift $ ensureIdentLinearity m S.empty $ map snd noeticVarList
        delimiter "="
        e1 <- rawExpr
        delimiter "in"
        return $ \e2 -> m :< RT.Let mxt noeticVarList e1 (modifier isNoetic e2),
      do
        delimiter "="
        e1 <- rawExpr
        delimiter "in"
        return $ \e2 -> m :< RT.Let mxt [] e1 (modifier isNoetic e2)
    ]

getContinuationModifier :: (Hint, RP.RawPattern) -> Parser (RawIdent, N.IsNoetic -> RT.RawTerm -> RT.RawTerm)
getContinuationModifier pat =
  case pat of
    (_, RP.Var (Var x)) ->
      return (x, \_ e -> e)
    _ -> do
      tmp <- lift $ Gensym.newTextFromText "tmp"
      return (tmp, \isNoetic e@(m :< _) -> m :< RT.DataElim isNoetic [rawVar m (Var tmp)] (RP.new [(V.fromList [pat], e)]))

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
  return $ bind (m, tmp, t) e (rawVar m (Var tmp))

rawTermLetVarAscription' :: Parser (Maybe RT.RawTerm)
rawTermLetVarAscription' =
  choice
    [ try $ do
        delimiter ":"
        Just <$> rawTerm,
      return Nothing
    ]

ensureIdentLinearity :: Hint -> S.Set RawIdent -> [RawIdent] -> App ()
ensureIdentLinearity m foundVarSet vs =
  case vs of
    [] ->
      return ()
    name : rest
      | S.member name foundVarSet ->
          Throw.raiseError m $ "found a non-linear occurrence of `" <> name <> "`."
      | otherwise ->
          ensureIdentLinearity m (S.insert name foundVarSet) rest

rawTermNoeticVar :: Parser (Hint, T.Text)
rawTermNoeticVar = do
  (m, x) <- var
  return (m, x)

rawTermLetExcept :: Parser (RT.RawTerm -> RT.RawTerm)
rawTermLetExcept = do
  keyword "let?"
  pat@(mx, _) <- rawTermPattern
  rightType <- rawTermLetVarAscription mx
  delimiter "="
  e1@(m1 :< _) <- rawExpr
  delimiter "in"
  eitherTypeInner <- lift $ locatorToVarGlobal m1 coreExcept
  leftType <- lift $ Gensym.newPreHole m1
  let eitherType = m1 :< RT.PiElim eitherTypeInner [leftType, rightType]
  e1' <- ascribe m1 eitherType e1
  m2 <- getCurrentHint
  err <- lift Gensym.newText
  exceptFail <- lift $ locatorToName m2 coreExceptFail
  exceptPass <- lift $ locatorToName m2 coreExceptPass
  exceptFailVar <- lift $ locatorToVarGlobal m2 coreExceptFail
  return $ \e2 ->
    m2
      :< RT.DataElim
        False
        [e1']
        ( RP.new
            [ ( V.fromList [(m2, RP.Cons exceptFail (RP.Paren [(m2, RP.Var (Var err))]))],
                m2 :< RT.PiElim exceptFailVar [preVar m2 err]
              ),
              (V.fromList [(m2, RP.Cons exceptPass (RP.Paren [pat]))], e2)
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

foldByOp :: Hint -> Name -> [RT.RawTerm] -> RT.RawTerm
foldByOp m op es =
  case es of
    [] ->
      error "RawTerm.foldByOp: invalid argument"
    [e] ->
      e
    e : rest ->
      m :< RT.PiElim (rawVar m op) [e, foldByOp m op rest]

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
  impDomArgList <- parseImplicitArgs
  expDomArgList <- argSeqOrList preBinder
  lift $ ensureArgumentLinearity S.empty $ map (\(mx, x, _) -> (mx, x)) expDomArgList
  argListList <- many $ argList preBinder
  codType <- parseDefInfoCod m
  e <- betweenBrace rawExpr
  let e' = foldPiIntro m argListList e
  let codType' = foldPi m argListList codType
  return ((m, funcBaseName), impDomArgList, expDomArgList, codType', e')

parseImplicitArgs :: Parser [RawBinder RT.RawTerm]
parseImplicitArgs =
  choice
    [ do
        betweenBracket (commaList preBinder),
      return []
    ]

foldPi :: Hint -> [[RawBinder RT.RawTerm]] -> RT.RawTerm -> RT.RawTerm
foldPi m args t =
  case args of
    [] ->
      t
    binder : rest ->
      m :< RT.Pi binder (foldPi m rest t)

foldPiIntro :: Hint -> [[RawBinder RT.RawTerm]] -> RT.RawTerm -> RT.RawTerm
foldPiIntro m args e =
  case args of
    [] ->
      e
    binder : rest ->
      lam m binder (foldPiIntro m rest e)

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

rawTermMu :: Parser RT.RawTerm
rawTermMu = do
  m <- getCurrentHint
  keyword "mu"
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
  isNoetic <- choice [try (keyword "&match") >> return True, keyword "match" >> return False]
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
      try rawTermPatternOptionSome,
      try rawTermPatternOptionNone,
      rawTermPatternConsOrVar
    ]

rawTermPatternOptionNone :: Parser (Hint, RP.RawPattern)
rawTermPatternOptionNone = do
  m <- getCurrentHint
  keyword "None"
  exceptFail <- lift $ locatorToName m coreExceptFail
  hole <- lift Gensym.newTextForHole
  return (m, RP.Cons exceptFail (RP.Paren [(m, RP.Var (Var hole))]))

rawTermPatternOptionSome :: Parser (Hint, RP.RawPattern)
rawTermPatternOptionSome = do
  m <- getCurrentHint
  keyword "Some"
  pat <- betweenParen rawTermPattern
  exceptPass <- lift $ locatorToName m coreExceptPass
  return (m, RP.Cons exceptPass (RP.Paren [pat]))

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
      (m, RP.Var unitVar)
    [e] ->
      e
    e : rest -> do
      let rest' = foldTuplePat m unitVar pairVar rest
      (m, RP.Cons pairVar (RP.Paren [e, rest']))

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
  boolTrue <- lift $ locatorToName m coreBoolTrue
  boolFalse <- lift $ locatorToName m coreBoolFalse
  return $ foldIf m boolTrue boolFalse ifCond ifBody elseIfList elseBody

rawTermWhen :: Parser RT.RawTerm
rawTermWhen = do
  m <- getCurrentHint
  keyword "when"
  whenCond <- rawTerm
  whenBody <- betweenBrace rawExpr
  boolTrue <- lift $ locatorToName m coreBoolTrue
  boolFalse <- lift $ locatorToName m coreBoolFalse
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
  unitVar <- lift $ locatorToName m coreUnit
  pairVar <- lift $ locatorToName m corePair
  case es of
    [] ->
      return $ rawVar m unitVar
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
  return $ \e2 -> m :< RT.PiElim binder [e1, lam m [mxt] (modifier False e2)]

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
  t <- lift $ Gensym.newPreHole m
  detachVar <- lift $ locatorToVarGlobal m coreThreadDetach
  e <- rawTermSimple
  return $ m :< RT.PiElim detachVar [t, lam m [] e]

rawTermFlowElim :: Parser RT.RawTerm
rawTermFlowElim = do
  m <- getCurrentHint
  keyword "attach"
  t <- lift $ Gensym.newPreHole m
  attachVar <- lift $ locatorToVarGlobal m coreThreadAttach
  e <- rawTermSimple
  return $ m :< RT.PiElim attachVar [t, e]

rawTermOption :: Parser RT.RawTerm
rawTermOption = do
  m <- getCurrentHint
  delimiter "?"
  t <- rawTermBasic
  optionVar <- lift $ locatorToVarGlobal m coreExceptOption
  return $ m :< RT.PiElim optionVar [t]

rawTermOptionNone :: Parser RT.RawTerm
rawTermOptionNone = do
  m <- getCurrentHint
  keyword "None"
  noneVar <- lift $ locatorToVarGlobal m coreExceptNoneInternal
  t <- lift $ Gensym.newPreHole m
  return $ m :< RT.PiElim noneVar [t]

rawTermOptionSome :: Parser RT.RawTerm
rawTermOptionSome = do
  m <- getCurrentHint
  keyword "Some"
  e <- betweenParen rawExpr
  someVar <- lift $ locatorToVarGlobal m coreExceptSomeInternal
  t <- lift $ Gensym.newPreHole m
  return $ m :< RT.PiElim someVar [t, e]

rawTermAdmit :: Parser RT.RawTerm
rawTermAdmit = do
  m <- getCurrentHint
  keyword "admit"
  admit <- lift $ locatorToVarGlobal m coreSystemAdmit
  t <- lift $ Gensym.newPreHole m
  textType <- lift $ locatorToVarGlobal m coreText
  return $
    m
      :< RT.Annotation
        Warning
        (Annot.Type ())
        ( m
            :< RT.PiElim
              admit
              [t, m :< RT.Prim (WP.Value (WPV.StaticText textType ("admit: " <> T.pack (toString m) <> "\n")))]
        )

rawTermAssert :: Parser RT.RawTerm
rawTermAssert = do
  m <- getCurrentHint
  keyword "assert"
  message <- string
  e@(mCond :< _) <- betweenBrace rawExpr
  assert <- lift $ locatorToVarGlobal m coreSystemAssert
  textType <- lift $ locatorToVarGlobal m coreText
  let fullMessage = T.pack (toString m) <> "\nassertion failure: " <> message <> "\n"
  return $
    m
      :< RT.PiElim
        assert
        [m :< RT.Prim (WP.Value (WPV.StaticText textType fullMessage)), lam mCond [] e]

rawTermPiElimOrSimple :: Parser RT.RawTerm
rawTermPiElimOrSimple = do
  m <- getCurrentHint
  e <- rawTermSimple
  mImpArgNum <- optional $ delimiter "/" >> integer
  case e of
    _ :< RT.Var attr name -> do
      choice
        [ do
            holes <- lift $ mapM (const $ Gensym.newPreHole m) [1 .. fromMaybe 0 mImpArgNum]
            keyword "of"
            rowList <- betweenBrace $ manyList rawTermKeyValuePair
            return $ m :< RT.PiElimByKey attr name holes rowList,
          rawTermPiElimCont m e mImpArgNum
        ]
    _ -> do
      rawTermPiElimCont m e mImpArgNum

rawTermPiElimCont :: Hint -> RT.RawTerm -> Maybe Integer -> Parser RT.RawTerm
rawTermPiElimCont m e mImpArgNum = do
  argListList <- many $ argList rawExpr
  case mImpArgNum of
    Just impArgNum -> do
      holes <- lift $ mapM (const $ Gensym.newPreHole m) [1 .. impArgNum]
      case argListList of
        [] ->
          return $ m :< RT.PiElim e holes
        headArgList : rest ->
          foldPiElim m e $ (holes ++ headArgList) : rest
    Nothing ->
      foldPiElim m e argListList

foldPiElim :: Hint -> RT.RawTerm -> [[RT.RawTerm]] -> Parser RT.RawTerm
foldPiElim m e argListList =
  case argListList of
    [] ->
      return e
    args : rest ->
      foldPiElim m (m :< RT.PiElim e args) rest

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
      m :< RT.PiElim listCons [e, foldListApp m listNil listCons rest]

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
  (isExplicit, (m, varOrLocator)) <- parseVarName
  return $ m :< RT.Var (AttrV.Attr {..}) varOrLocator

parseVarName :: Parser (IsExplicit, (Hint, Name))
parseVarName = do
  (m, varText) <- var
  case T.uncons varText of
    Just ('@', varText') ->
      interpretVarName m varText' >>= \value -> return (True, value)
    _ ->
      interpretVarName m varText >>= \value -> return (False, value)

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
  m :< RT.PiIntro LK.Normal varList e

preVar :: Hint -> T.Text -> RT.RawTerm
preVar m str =
  rawVar m (Var str)

locatorToName :: Hint -> T.Text -> App Name
locatorToName m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair m text
  return $ Locator (gl, ll)

locatorToVarGlobal :: Hint -> T.Text -> App RT.RawTerm
locatorToVarGlobal m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair m text
  return $ rawVar m (Locator (gl, ll))

rawVar :: Hint -> Name -> RT.RawTerm
rawVar m name =
  m :< RT.Var (AttrV.Attr {isExplicit = False}) name
