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
import Entity.BuildMode qualified as BM
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

rawExpr :: Parser RT.RawTerm
rawExpr = do
  m <- getCurrentHint
  choice
    [ rawExprLet m,
      rawExprLink m,
      rawExprSeqOrTerm m
    ]

rawExprLet :: Hint -> Parser RT.RawTerm
rawExprLet m = do
  choice
    [ try rawTermLetEither,
      rawTermLetOrLetOn m
    ]

rawExprSeqOrTerm :: Hint -> Parser RT.RawTerm
rawExprSeqOrTerm m = do
  e1 <- rawTerm
  choice
    [ do
        notFollowedBy "-"
        e2 <- rawExpr
        f <- lift Gensym.newTextForHole
        unit <- lift $ locatorToVarGlobal m coreUnit
        return $ bind (m, f, unit) e1 e2,
      return e1
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
      rawTermMatchNoetic,
      rawTermMatch,
      rawTermIf,
      rawTermWhen,
      rawTermAssert,
      rawTermNoema,
      rawTermFlow,
      rawTermFlowIntro,
      rawTermFlowElim,
      rawTermOption,
      rawTermOptionNone,
      rawTermOptionSome,
      rawTermEmbody,
      rawTermTuple,
      rawTermTupleIntro,
      rawTermPiElimOrSimple
    ]

{-# INLINE rawTermSimple' #-}
rawTermSimple' :: Parser RT.RawTerm
rawTermSimple' = do
  choice
    [ rawTermBrace',
      rawTermListIntro,
      rawTermTextIntro',
      rawTermTau',
      rawTermAdmit',
      rawTermHole',
      rawTermInteger',
      rawTermFloat',
      rawTermSymbol'
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
      case basic of
        _ :< RT.Var name -> do
          choice
            [ do
                keyword "of"
                rowList <- betweenBrace $ manyList rawTermKeyValuePair
                return $ m :< RT.PiElimByKey name rowList,
              return basic
            ]
        _ ->
          return basic
    ]

rawTermKeyValuePair :: Parser (Hint, Key, RT.RawTerm)
rawTermKeyValuePair = do
  (m, key) <- var
  delimiter "=>"
  value <- rawExpr
  return (m, key, value)

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
        lift $ ensureIdentLinearity m S.empty $ map snd noeticVarList
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
getContinuationModifier pat =
  case pat of
    (_, RP.Var (Var x)) ->
      return (x, \_ e -> e)
    _ -> do
      tmp <- lift $ Gensym.newTextFromText "tmp"
      return (tmp, \isNoetic e@(m :< _) -> m :< RT.DataElim isNoetic [m :< RT.Var (Var tmp)] (RP.new [(V.fromList [pat], e)]))

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

rawExprLink :: Hint -> Parser RT.RawTerm
rawExprLink m = do
  keyword "link"
  pat@(mx, _) <- rawTermPattern
  (x, modifier) <- getContinuationModifier pat
  t <- rawTermLetVarAscription mx
  delimiter "="
  e1 <- rawTerm
  e2 <- rawExpr
  return $ m :< RT.Let (mx, x, t) [] e1 (modifier True e2)

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

-- let? x = e1 e2
rawTermLetEither :: Parser RT.RawTerm
rawTermLetEither = do
  keyword "let?"
  pat@(mx, _) <- rawTermPattern
  rightType <- rawTermLetVarAscription mx
  delimiter "="
  e1@(m1 :< _) <- rawTerm
  eitherTypeInner <- lift $ locatorToVarGlobal m1 coreEither
  leftType <- lift $ Gensym.newPreHole m1
  let eitherType = m1 :< RT.PiElim eitherTypeInner [leftType, rightType]
  e1' <- ascribe m1 eitherType e1
  e2@(m2 :< _) <- rawExpr
  err <- lift Gensym.newText
  left <- lift $ locatorToName m2 coreEitherLeft
  right <- lift $ locatorToName m2 coreEitherRight
  leftVar <- lift $ locatorToVarGlobal m2 coreEitherLeft
  return $
    m2
      :< RT.DataElim
        False
        [e1']
        ( RP.new
            [ (V.fromList [(m2, RP.Cons left (Right [(m2, RP.Var (Var err))]))], m2 :< RT.PiElim leftVar [preVar m2 err]),
              (V.fromList [(m2, RP.Cons right (Right [pat]))], e2)
            ]
        )

rawTermEmbody :: Parser RT.RawTerm
rawTermEmbody = do
  m <- getCurrentHint
  delimiter "*"
  e <- rawTermBasic
  return $ m :< RT.Embody e

rawTermTau' :: Parser RT.RawTerm
rawTermTau' = do
  m <- getCurrentHint
  keyword' "tau"
  return $ m :< RT.Tau

rawTermHole' :: Parser RT.RawTerm
rawTermHole' = do
  m <- getCurrentHint
  keyword' "_"
  lift $ Gensym.newPreHole m

foldByOp :: Hint -> Name -> [RT.RawTerm] -> RT.RawTerm
foldByOp m op es =
  case es of
    [] ->
      error "RawTerm.foldByOp: invalid argument"
    [e] ->
      e
    e : rest ->
      m :< RT.PiElim (m :< RT.Var op) [e, foldByOp m op rest]

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
  domInfoList <- argSeqOrList preBinder
  lift $ ensureArgumentLinearity S.empty $ map (\(mx, x, _) -> (mx, x)) domInfoList
  additionalDomInfoList <- many $ argList preBinder
  codType <- parseDefInfoCod m
  e <- betweenBrace rawExpr
  let codType' = foldPi m additionalDomInfoList codType
  let e' = foldLam m additionalDomInfoList e
  return ((m, funcBaseName), impDomInfoList, domInfoList, codType', e')

foldLam :: Hint -> [[RawBinder RT.RawTerm]] -> RT.RawTerm -> RT.RawTerm
foldLam m domInfoList e =
  case domInfoList of
    [] ->
      e
    domInfo : rest ->
      lam m domInfo $ foldLam m rest e

foldPi :: Hint -> [[RawBinder RT.RawTerm]] -> RT.RawTerm -> RT.RawTerm
foldPi m domInfoList e =
  case domInfoList of
    [] ->
      e
    domInfo : rest ->
      m :< RT.Pi domInfo (foldPi m rest e)

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
    extFunName <- symbol
    es <- many (delimiter "," >> rawTerm)
    return $ m :< RT.Magic (M.External (EN.ExternalName extFunName) es)

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
    intValue <- lexeme integer'
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
  es <- commaList rawTermPiOrConsOrAscOrBasic
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
        delimiter "::"
        pat <- rawTermPattern
        listCons <- lift $ locatorToName m coreListCons
        return (m, RP.Cons listCons (Right [headPat, pat])),
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
  left <- lift $ locatorToName m coreEitherLeft
  hole <- lift Gensym.newTextForHole
  return (m, RP.Cons left (Right [(m, RP.Var (Var hole))]))

rawTermPatternOptionSome :: Parser (Hint, RP.RawPattern)
rawTermPatternOptionSome = do
  m <- getCurrentHint
  keyword "Some"
  pat <- betweenParen rawTermPattern
  right <- lift $ locatorToName m coreEitherRight
  return (m, RP.Cons right (Right [pat]))

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
      (m, RP.Cons listCons (Right [e, rest']))

rawTermPatternTupleIntro :: Parser (Hint, RP.RawPattern)
rawTermPatternTupleIntro = do
  m <- getCurrentHint
  keyword "Tuple"
  patList <- betweenParen $ commaList rawTermPattern
  unitVar <- lift $ locatorToName m coreUnitUnit
  bothVar <- lift $ locatorToName m coreBothBoth
  return $ foldTuplePat m unitVar bothVar patList

foldTuplePat :: Hint -> Name -> Name -> [(Hint, RP.RawPattern)] -> (Hint, RP.RawPattern)
foldTuplePat m unitVar bothVar es =
  case es of
    [] ->
      (m, RP.Var unitVar)
    [e] ->
      e
    e : rest -> do
      let rest' = foldTuplePat m unitVar bothVar rest
      (m, RP.Cons bothVar (Right [e, rest']))

parseName :: Parser (Hint, Name)
parseName = do
  lexeme parseName'

parseName' :: Parser (Hint, Name)
parseName' = do
  (m, varText) <- var'
  case DD.getLocatorPair m varText of
    Left _ ->
      return (m, Var varText)
    Right (gl, ll) ->
      return (m, Locator (gl, ll))

rawTermPatternConsOrVar :: Parser (Hint, RP.RawPattern)
rawTermPatternConsOrVar = do
  (m, varOrLocator) <- parseName
  choice
    [ try $ do
        mVar <- betweenParen $ getCurrentHint <* delimiter ".."
        return (m, RP.Cons varOrLocator (Left mVar)),
      do
        patArgs <- argList rawTermPattern
        return (m, RP.Cons varOrLocator (Right patArgs)),
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

rawTermBrace' :: Parser RT.RawTerm
rawTermBrace' =
  betweenBrace' rawExpr

rawTermTuple :: Parser RT.RawTerm
rawTermTuple = do
  m <- getCurrentHint
  keyword "tuple"
  es <- betweenParen $ commaList rawExpr
  unitVar <- lift $ locatorToName m coreUnit
  bothVar <- lift $ locatorToName m coreBoth
  case es of
    [] ->
      return $ m :< RT.Var unitVar
    [e] ->
      return e
    _ ->
      return $ foldByOp m bothVar es

rawTermTupleIntro :: Parser RT.RawTerm
rawTermTupleIntro = do
  m <- getCurrentHint
  keyword "Tuple"
  es <- betweenParen $ commaList rawExpr
  unitVar <- lift $ locatorToName m coreUnitUnit
  bothVar <- lift $ locatorToName m coreBothBoth
  case es of
    [] ->
      return $ m :< RT.Var unitVar
    [e] ->
      return e
    _ ->
      return $ foldByOp m bothVar es

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
  e <- lexeme rawTermSimple'
  return $ m :< RT.FlowIntro flowVar detachVar e

rawTermFlowElim :: Parser RT.RawTerm
rawTermFlowElim = do
  m <- getCurrentHint
  keyword "attach"
  flowVar <- lift $ Throw.liftEither $ DD.getLocatorPair m coreThreadFlowInner
  attachVar <- lift $ Throw.liftEither $ DD.getLocatorPair m coreThreadAttach
  e <- lexeme rawTermSimple'
  return $ m :< RT.FlowElim flowVar attachVar e

rawTermOption :: Parser RT.RawTerm
rawTermOption = do
  m <- getCurrentHint
  delimiter "?"
  t <- rawTermBasic
  optionVar <- lift $ locatorToVarGlobal m coreEitherOption
  return $ m :< RT.PiElim optionVar [t]

rawTermOptionNone :: Parser RT.RawTerm
rawTermOptionNone = do
  m <- getCurrentHint
  keyword "None"
  noneVar <- lift $ locatorToVarGlobal m coreEitherNoneInternal
  return $ m :< RT.PiElim noneVar []

rawTermOptionSome :: Parser RT.RawTerm
rawTermOptionSome = do
  m <- getCurrentHint
  keyword "Some"
  e <- betweenParen rawExpr
  someVar <- lift $ locatorToVarGlobal m coreEitherSomeInternal
  return $ m :< RT.PiElim someVar [e]

rawTermAdmit' :: Parser RT.RawTerm
rawTermAdmit' = do
  m <- getCurrentHint
  keyword' "admit"
  admit <- lift $ locatorToVarGlobal m coreSystemAdmit
  textType <- lift $ locatorToVarGlobal m coreText
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
  e <- rawTermSimple'
  argListList <- many $ argList' rawExpr
  spaceConsumer
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
  tp <- Env.getTargetPlatform
  bm <- Env.getBuildMode
  case key of
    "target-platform" -> do
      return $ TP.reify tp
    "target-arch" ->
      return $ Arch.reify (TP.arch tp)
    "target-os" ->
      return $ OS.reify (TP.os tp)
    "build-mode" ->
      return $ BM.reify bm
    _ ->
      Throw.raiseError m $ "no such introspective value is defined: " <> key

rawTermSymbol' :: Parser RT.RawTerm
rawTermSymbol' = do
  (m, varOrLocator) <- parseName'
  return $ m :< RT.Var varOrLocator

rawTermTextIntro' :: Parser RT.RawTerm
rawTermTextIntro' = do
  m <- getCurrentHint
  s <- string'
  textType <- lift $ locatorToVarGlobal m coreText
  return $ m :< RT.Prim (WP.Value (WPV.StaticText textType s))

rawTermInteger' :: Parser RT.RawTerm
rawTermInteger' = do
  m <- getCurrentHint
  intValue <- try integer'
  h <- lift $ Gensym.newPreHole m
  return $ m :< RT.Prim (WP.Value (WPV.Int h intValue))

rawTermFloat' :: Parser RT.RawTerm
rawTermFloat' = do
  m <- getCurrentHint
  floatValue <- try float'
  h <- lift $ Gensym.newPreHole m
  return $ m :< RT.Prim (WP.Value (WPV.Float h floatValue))

lam :: Hint -> [RawBinder RT.RawTerm] -> RT.RawTerm -> RT.RawTerm
lam m varList e =
  m :< RT.PiIntro (LK.Normal O.Transparent) varList e

preVar :: Hint -> T.Text -> RT.RawTerm
preVar m str =
  m :< RT.Var (Var str)

locatorToName :: Hint -> T.Text -> App Name
locatorToName m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair m text
  return $ Locator (gl, ll)

locatorToVarGlobal :: Hint -> T.Text -> App RT.RawTerm
locatorToVarGlobal m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair m text
  return $ m :< RT.Var (Locator (gl, ll))
