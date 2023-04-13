module Scene.Parse.RawTerm
  ( rawExpr,
    rawTerm,
    preAscription,
    preBinder,
    parseTopDefInfo,
    preVar,
  )
where

import Codec.Binary.UTF8.String
import Context.App
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Trans
import Data.Bifunctor
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.Binder
import Entity.Const
import Entity.DefiniteDescription qualified as DD
import Entity.ExternalName qualified as EN
import Entity.GlobalLocator qualified as GL
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reflect qualified as Ident
import Entity.LamKind qualified as LK
import Entity.LocalLocator qualified as LL
import Entity.LowType qualified as LT
import Entity.Magic qualified as M
import Entity.Mutability
import Entity.Opacity qualified as O
import Entity.PrimNumSize qualified as PNS
import Entity.PrimType qualified as PT
import Entity.PrimType.FromText qualified as PT
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.TargetPlatform qualified as TP
import Entity.UnresolvedName qualified as UN
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Scene.Parse.Core
import Text.Megaparsec
import Text.Megaparsec.Char (char)

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
    [ try $ rawTermLetCoproduct m,
      rawTermLetOrLetOn m
    ]

rawExprSeqOrTerm :: Hint -> Parser RT.RawTerm
rawExprSeqOrTerm m = do
  e1 <- rawTerm
  choice
    [ do
        e2 <- rawExpr
        f <- lift $ Gensym.newTextualIdentFromText "unit"
        top <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreTop
        return $ bind (m, f, m :< RT.PiElim top []) e1 e2,
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
      rawTermNew,
      rawTermCell,
      rawTermIf,
      rawTermListIntro,
      rawTermPiGeneral,
      rawTermPiOrConsOrAscOrBasic
    ]

rawTermBasic :: Parser RT.RawTerm
rawTermBasic = do
  choice
    [ rawTermNoema,
      rawTermOption,
      rawTermEmbody,
      rawTermLazy,
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
      rawTermVarOrDefiniteDescription
    ]

rawTermLetOrLetOn :: Hint -> Parser RT.RawTerm
rawTermLetOrLetOn m = do
  keyword "let"
  pat <- rawTermPattern
  case pat of
    (mx, RP.Var x) -> do
      t <- rawTermLetVarAscription mx
      let mxt = (mx, x, t)
      choice
        [ do
            keyword "on"
            noeticVarList <- map (second Ident.fromText) <$> commaList rawTermNoeticVar
            lift $ ensureNoeticVarLinearity m S.empty $ map (\(_, _, v) -> v) noeticVarList
            delimiter "="
            e1 <- rawTerm
            e2 <- rawExpr
            return $ m :< RT.Let mxt noeticVarList e1 e2,
          do
            delimiter "="
            e1 <- rawTerm
            e2 <- rawExpr
            return $ m :< RT.Let mxt [] e1 e2
        ]
    _ -> do
      mt <- rawTermLetVarAscription'
      delimiter "="
      e1 <- rawTerm
      e2 <- rawExpr
      e1' <- annotateIfNecessary m mt e1
      return $ m :< RT.DataElim False [e1'] (RP.new [(V.fromList [pat], e2)])

rawTermLetVarAscription :: Hint -> Parser RT.RawTerm
rawTermLetVarAscription m = do
  mt <- rawTermLetVarAscription'
  case mt of
    Just t ->
      return t
    Nothing ->
      lift $ Gensym.newPreHole m

annotateIfNecessary :: Hint -> Maybe RT.RawTerm -> RT.RawTerm -> Parser RT.RawTerm
annotateIfNecessary m mt e =
  case mt of
    Just t -> do
      tmp <- lift $ Gensym.newTextualIdentFromText "tmp"
      return $ bind (m, tmp, t) e (m :< RT.Var tmp)
    Nothing ->
      return e

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
  pat <- rawTermPattern
  delimiter "="
  e1 <- rawTerm
  e2 <- rawExpr
  return $ m :< RT.DataElim True [e1] (RP.new [(V.fromList [pat], e2)])

ensureNoeticVarLinearity :: Hint -> S.Set T.Text -> [Ident] -> App ()
ensureNoeticVarLinearity m foundVarSet vs =
  case vs of
    [] ->
      return ()
    I (name, _) : rest
      | S.member name foundVarSet ->
          Throw.raiseError m $ "found a non-linear occurrence of `" <> name <> "`."
      | otherwise ->
          ensureNoeticVarLinearity m (S.insert name foundVarSet) rest

rawTermNoeticVar :: Parser (Mutability, Hint, T.Text)
rawTermNoeticVar =
  choice
    [ do
        keyword "mutable"
        (m, x) <- var
        return (Mutable, m, x),
      do
        (m, x) <- var
        return (Immutable, m, x)
    ]

-- let? x = e1 in e2
rawTermLetCoproduct :: Hint -> Parser RT.RawTerm
rawTermLetCoproduct m = do
  keyword "let?"
  x <- Ident.fromText <$> symbol
  delimiter "="
  e1 <- rawTerm
  e2 <- rawExpr
  err <- lift $ Gensym.newTextualIdentFromText "err"
  sumLeft <- lift $ handleDefiniteDescriptionIntoRawConsName m coreSumLeft
  sumRight <- lift $ handleDefiniteDescriptionIntoRawConsName m coreSumRight
  sumLeftVar <- lift $ handleDefiniteDescriptionIntoVarGlobal m coreSumLeft
  return $
    m
      :< RT.DataElim
        False
        [e1]
        ( RP.new
            [ (V.fromList [(m, RP.Cons sumLeft [(m, RP.Var err)])], m :< RT.PiElim sumLeftVar [preVar' m err]),
              (V.fromList [(m, RP.Cons sumRight [(m, RP.Var x)])], e2)
            ]
        )

rawTermEmbody :: Parser RT.RawTerm
rawTermEmbody = do
  m <- getCurrentHint
  delimiter "*"
  e <- rawTermBasic
  return $ m :< RT.Embody e

rawTermLazy :: Parser RT.RawTerm
rawTermLazy = do
  m <- getCurrentHint
  delimiter "'"
  t <- rawTermBasic
  return $ m :< RT.Pi [] t

rawTermTau :: Parser RT.RawTerm
rawTermTau = do
  m <- getCurrentHint
  keyword "tau"
  return $ m :< RT.Tau

rawTermHole :: Parser RT.RawTerm
rawTermHole = do
  m <- getCurrentHint
  delimiter "_"
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
        x <- lift $ Gensym.newTextualIdentFromText "_"
        cod <- rawTerm
        return $ m :< RT.Pi [(m, x, basic)] cod,
      do
        delimiter "::"
        rest <- rawTerm
        return $ m :< RT.PiElim (m :< RT.Var (Ident.fromText "list.cons")) [basic, rest],
      do
        delimiter ":"
        t <- rawTerm
        annotateIfNecessary m (Just t) basic,
      return basic
    ]

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
  let piType = mFun :< RT.Pi domBinderList codType
  return $ m :< RT.PiIntro (LK.Fix (mFun, Ident.fromText functionName, piType)) domBinderList e

parseDefiniteDescription :: Parser (Hint, GL.GlobalLocator, LL.LocalLocator)
parseDefiniteDescription = do
  notFollowedBy (char '-')
  m <- getCurrentHint
  baseSymbol <- definiteDescriptionSymbol
  let (beforeSep, rest) = T.breakOn definiteSep baseSymbol
  if T.null rest
    then failure (Just (asTokens baseSymbol)) (S.fromList [asLabel "definite description"])
    else do
      let afterSep = T.drop (T.length definiteSep) rest
      globalLocator <- lift $ Throw.liftEither $ GL.reflect m beforeSep
      localLocator <- lift $ Throw.liftEither $ LL.reflect m afterSep
      return (m, globalLocator, localLocator)

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
  sizeString <- symbol
  case PT.fromText sizeString of
    Just primNum ->
      return primNum
    _ -> do
      failure (Just (asTokens sizeString)) (S.fromList [asLabel "i{n}", asLabel "f{n}"])

rawTermMatch :: Parser RT.RawTerm
rawTermMatch = do
  m <- getCurrentHint
  keyword "match"
  es <- commaList rawTerm
  patternRowList <- betweenBrace $ manyList $ rawTermPatternRow (length es)
  return $ m :< RT.DataElim False es (RP.new patternRowList)

rawTermMatchNoetic :: Parser RT.RawTerm
rawTermMatchNoetic = do
  m <- getCurrentHint
  keyword "case"
  es <- commaList rawTerm
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
        return (m, RP.Cons (RP.UnresolvedName $ UN.UnresolvedName "list.cons") [headPat, pat]),
      return headPat
    ]

rawTermPatternBasic :: Parser (Hint, RP.RawPattern)
rawTermPatternBasic =
  choice
    [ rawTermPatternListIntro,
      rawTermPatternConsOrVar
    ]

rawTermPatternListIntro :: Parser (Hint, RP.RawPattern)
rawTermPatternListIntro = do
  m <- getCurrentHint
  patList <- betweenBracket $ commaList rawTermPattern
  return $ foldListAppPat m patList

foldListAppPat :: Hint -> [(Hint, RP.RawPattern)] -> (Hint, RP.RawPattern)
foldListAppPat m es =
  case es of
    [] ->
      (m, RP.Cons (RP.UnresolvedName $ UN.UnresolvedName "list.nil") []) -- nil
    e : rest -> do
      let rest' = foldListAppPat m rest
      (m, RP.Cons (RP.UnresolvedName $ UN.UnresolvedName "list.cons") [e, rest'])

parseVarOrDefiniteDescription :: Parser (Either (Hint, T.Text) (Hint, GL.GlobalLocator, LL.LocalLocator))
parseVarOrDefiniteDescription = do
  choice
    [ try $ do
        (m, globalLocator, localLocator) <- parseDefiniteDescription
        return $ Right (m, globalLocator, localLocator),
      do
        (m, x) <- var
        return $ Left (m, x)
    ]

rawTermPatternConsOrVar :: Parser (Hint, RP.RawPattern)
rawTermPatternConsOrVar = do
  varOrDefiniteDescription <- parseVarOrDefiniteDescription
  choice
    [ do
        patArgs <- argList rawTermPattern
        case varOrDefiniteDescription of
          Left (m, c) -> do
            return (m, RP.Cons (RP.UnresolvedName $ UN.UnresolvedName c) patArgs)
          Right (m, globalLocator, localLocator) -> do
            return (m, RP.Cons (RP.LocatorPair globalLocator localLocator) patArgs),
      do
        case varOrDefiniteDescription of
          Left (m, c) ->
            return (m, RP.Var (Ident.fromText c))
          Right (m, _, _) ->
            lift $ Throw.raiseError m "found a raw definite description in a pattern"
    ]

rawTermNew :: Parser RT.RawTerm
rawTermNew = do
  m <- getCurrentHint
  keyword "new"
  name <- symbol
  rowList <- betweenBrace $ manyList rawTermNewRow
  return $ m :< RT.New name rowList

rawTermNewRow :: Parser (Hint, T.Text, RT.RawTerm)
rawTermNewRow = do
  m <- getCurrentHint
  key <- symbol
  delimiter "<="
  value <- rawExpr
  return (m, key, value)

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
  boolTrue <- lift $ handleDefiniteDescriptionIntoRawConsName m coreBoolTrue
  boolFalse <- lift $ handleDefiniteDescriptionIntoRawConsName m coreBoolFalse
  return $ foldIf m boolTrue boolFalse ifCond ifBody elseIfList elseBody

foldIf ::
  Hint ->
  RP.RawConsName ->
  RP.RawConsName ->
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
              [ (V.fromList [(m, RP.Cons true [])], ifBody),
                (V.fromList [(m, RP.Cons false [])], elseBody)
              ]
          )
    ((elseIfCond, elseIfBody) : rest) -> do
      let cont = foldIf m true false elseIfCond elseIfBody rest elseBody
      m
        :< RT.DataElim
          False
          [ifCond]
          ( RP.new
              [ (V.fromList [(m, RP.Cons true [])], ifBody),
                (V.fromList [(m, RP.Cons false [])], cont)
              ]
          )

rawTermBrace :: Parser RT.RawTerm
rawTermBrace =
  betweenBrace rawExpr

bind :: BinderF RT.RawTerm -> RT.RawTerm -> RT.RawTerm -> RT.RawTerm
bind mxt@(m, _, _) e cont =
  m :< RT.Let mxt [] e cont

rawTermNoema :: Parser RT.RawTerm
rawTermNoema = do
  m <- getCurrentHint
  delimiter "&"
  t <- rawTermBasic
  return $ m :< RT.Noema t

rawTermCell :: Parser RT.RawTerm
rawTermCell = do
  m <- getCurrentHint
  delimiter "cell"
  t <- rawTerm
  return $ m :< RT.Cell t

rawTermOption :: Parser RT.RawTerm
rawTermOption = do
  m <- getCurrentHint
  delimiter "?"
  t <- rawTermBasic
  return $ m :< RT.PiElim (m :< RT.Var (Ident.fromText "option")) [t]

rawTermAdmit :: Parser RT.RawTerm
rawTermAdmit = do
  m <- getCurrentHint
  keyword "admit"
  h <- lift $ Gensym.newPreHole m
  return $
    m
      :< RT.PiElim
        (preVar m "core.os.exit")
        [ h,
          m :< RT.Prim (WP.Value (WPV.Int (RT.i64 m) 1))
        ]

rawTermPiElimOrSimple :: Parser RT.RawTerm
rawTermPiElimOrSimple = do
  m <- getCurrentHint
  e <- rawTermSimple
  elems <- many $ choice [rawTermPiElimForwardBracket, rawTermPiElimBackwardBracket]
  foldPiElimBracket m e elems

data PiElimBracket
  = PiElimBracketForward [RT.RawTerm] -- f<imp-arg-1, ..., imp-arg-n>(arg-1, ..., arg-m)
  | PiElimBracketBackward RT.RawTerm -- e[f]

rawTermPiElimForwardBracket :: Parser PiElimBracket
rawTermPiElimForwardBracket = do
  es <- argList rawTerm
  return $ PiElimBracketForward es

rawTermPiElimBackwardBracket :: Parser PiElimBracket
rawTermPiElimBackwardBracket = do
  f <- betweenBracket rawTerm
  return $ PiElimBracketBackward f

foldPiElimBracket :: Hint -> RT.RawTerm -> [PiElimBracket] -> Parser RT.RawTerm
foldPiElimBracket m e elemList =
  case elemList of
    [] ->
      return e
    bracket : rest ->
      case bracket of
        PiElimBracketForward args ->
          foldPiElimBracket m (m :< RT.PiElim e args) rest
        PiElimBracketBackward func ->
          foldPiElimBracket m (m :< RT.PiElim func [e]) rest

--
-- term-related helper functions
--

preBinder :: Parser (BinderF RT.RawTerm)
preBinder =
  choice
    [ try preAscription,
      preAscription'
    ]

preAscription :: Parser (BinderF RT.RawTerm)
preAscription = do
  (m, x) <- var
  delimiter ":"
  a <- rawTerm
  return (m, Ident.fromText x, a)

typeWithoutIdent :: Parser (BinderF RT.RawTerm)
typeWithoutIdent = do
  m <- getCurrentHint
  x <- lift $ Gensym.newTextualIdentFromText "_"
  t <- rawTerm
  return (m, x, t)

preAscription' :: Parser (BinderF RT.RawTerm)
preAscription' = do
  (m, x) <- preSimpleIdent
  h <- lift $ Gensym.newPreHole m
  return (m, x, h)

preSimpleIdent :: Parser (Hint, Ident)
preSimpleIdent = do
  (m, x) <- var
  return (m, Ident.fromText x)

rawTermListIntro :: Parser RT.RawTerm
rawTermListIntro = do
  m <- getCurrentHint
  es <- betweenBracket $ commaList rawTerm
  return $ foldListApp m es

foldListApp :: Hint -> [RT.RawTerm] -> RT.RawTerm
foldListApp m es =
  case es of
    [] ->
      m :< RT.PiElim (m :< RT.Var (Ident.fromText "list.nil")) []
    e : rest ->
      m :< RT.PiElim (m :< RT.Var (Ident.fromText "list.cons")) [e, foldListApp m rest]

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
      lift $ Throw.raiseError m $ "a clause for `" <> value <> "` is missing"

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
      return $ T.pack (TP.platform tp)
    "target-arch" ->
      return $ T.pack (TP.arch tp)
    "target-os" ->
      return $ T.pack (TP.os tp)
    _ ->
      Throw.raiseError m $ "no such introspective value is defined: " <> key

rawTermVarOrDefiniteDescription :: Parser RT.RawTerm
rawTermVarOrDefiniteDescription = do
  varOrDefiniteDescription <- parseVarOrDefiniteDescription
  case varOrDefiniteDescription of
    Left (m, x) ->
      return (preVar m x)
    Right (m, globalLocator, localLocator) ->
      return $ m :< RT.VarGlobal globalLocator localLocator

rawTermTextIntro :: Parser RT.RawTerm
rawTermTextIntro = do
  m <- getCurrentHint
  s <- string
  let i8s = encode $ T.unpack s
  let i8 = m :< RT.Prim (WP.Type (PT.Int (PNS.IntSize 8)))
  let i8s' = map (\x -> m :< RT.Prim (WP.Value (WPV.Int i8 (toInteger x)))) i8s
  return $ m :< RT.PiElim (m :< RT.Var (Ident.fromText "text-new")) [foldListApp m i8s']

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

lam :: Hint -> [BinderF RT.RawTerm] -> RT.RawTerm -> RT.RawTerm
lam m varList e =
  m :< RT.PiIntro (LK.Normal O.Transparent) varList e

preVar :: Hint -> T.Text -> RT.RawTerm
preVar m str =
  m :< RT.Var (Ident.fromText str)

preVar' :: Hint -> Ident -> RT.RawTerm
preVar' m ident =
  m :< RT.Var ident

handleDefiniteDescriptionIntoRawConsName :: Hint -> T.Text -> App RP.RawConsName
handleDefiniteDescriptionIntoRawConsName m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair m text
  return $ RP.LocatorPair gl ll

handleDefiniteDescriptionIntoVarGlobal :: Hint -> T.Text -> App RT.RawTerm
handleDefiniteDescriptionIntoVarGlobal m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair m text
  return $ m :< RT.VarGlobal gl ll
