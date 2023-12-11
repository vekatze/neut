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
    f,
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
import Data.Bifunctor
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.BaseName qualified as BN
import Entity.C
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

rawExpr :: Parser (RT.RawTerm, C)
rawExpr = do
  m <- getCurrentHint
  choice
    [ rawExprLet m,
      rawExprSeqOrTerm m
    ]

rawExprLet :: Hint -> Parser (RT.RawTerm, C)
rawExprLet m = do
  choice
    [ rawTermLet m,
      rawTermUse m
    ]

rawExprSeqOrTerm :: Hint -> Parser (RT.RawTerm, C)
rawExprSeqOrTerm m = do
  (e1, _) <- rawTerm
  choice
    [ do
        _ <- delimiter' ";"
        (e2, c2) <- rawExpr
        return (m :< RT.Seq e1 e2, c2),
      return (e1, [])
    ]

rawTerm :: Parser (RT.RawTerm, C)
rawTerm = do
  choice
    [ try rawTermPiGeneral,
      try rawTermPiIntro,
      rawTermPiOrConsOrAscOrBasic
    ]

rawTermBasic :: Parser (RT.RawTerm, C)
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
      rawTermWith,
      rawTermPiElimOrSimple
    ]

{-# INLINE rawTermSimple #-}
rawTermSimple :: Parser (RT.RawTerm, C)
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

rawTermPiGeneral :: Parser (RT.RawTerm, C)
rawTermPiGeneral = do
  m <- getCurrentHint
  impArgs <- map f <$> parseImplicitArgs
  expArgs <- map f <$> argList (choice [try preAscription, typeWithoutIdent])
  delimiter "->"
  (cod, c) <- rawTerm
  return (m :< RT.Pi impArgs expArgs cod, c)

rawTermPiIntro :: Parser (RT.RawTerm, C)
rawTermPiIntro = do
  m <- getCurrentHint
  impArgs <- map f <$> parseImplicitArgs
  expArgs <- map f <$> argList preBinder
  delimiter "=>"
  (e, c) <- rawExpr
  return (m :< RT.PiIntro LK.Normal impArgs expArgs e, c)

rawTermPiOrConsOrAscOrBasic :: Parser (RT.RawTerm, C)
rawTermPiOrConsOrAscOrBasic = do
  m <- getCurrentHint
  (basic, _) <- rawTermBasic
  choice
    [ do
        delimiter "->"
        x <- lift Gensym.newTextForHole
        (cod, c) <- rawTerm
        return (m :< RT.Pi [] [(m, x, basic)] cod, c),
      do
        delimiter ":"
        (t, _) <- rawTerm
        ascribe m t (basic, []),
      return (basic, [])
    ]

rawTermKeyValuePair :: Parser (Hint, Key, RT.RawTerm)
rawTermKeyValuePair = do
  ((m, key), _) <- var
  choice
    [ do
        delimiter "="
        (value, _) <- rawExpr
        return (m, key, value),
      do
        return (m, key, m :< RT.Var (Var key))
    ]

rawTermLet :: Hint -> Parser (RT.RawTerm, C)
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
  lift $ ensureIdentLinearity S.empty $ map fst noeticVarList
  let mxt = (mx, patInner, t)
  delimiter "="
  (e1, _) <- rawExpr
  delimiter "in"
  (e2, _) <- rawExpr
  return (mLet :< RT.Let letKind mxt (map fst noeticVarList) e1 e2, [])

rawTermUse :: Hint -> Parser (RT.RawTerm, C)
rawTermUse m = do
  keyword "use"
  (e, _) <- rawTerm
  xs <- betweenBrace $ commaList preBinder
  delimiter "in"
  lift $ ensureIdentLinearity S.empty $ map (\(mx, x, _) -> (mx, x)) xs
  (cont, _) <- rawExpr
  return (m :< RT.Use e (map f xs) cont, [])

rawTermLetVarAscription :: Hint -> Parser RT.RawTerm
rawTermLetVarAscription m = do
  mt <- rawTermLetVarAscription'
  case mt of
    Just t ->
      return t
    Nothing ->
      lift $ Gensym.newPreHole m

ascribe :: Hint -> RT.RawTerm -> (RT.RawTerm, C) -> Parser (RT.RawTerm, C)
ascribe m t (e, c) = do
  tmp <- lift Gensym.newTextForHole
  return (bind (m, tmp, t) e (rawVar m (Var tmp)), c)

rawTermLetVarAscription' :: Parser (Maybe RT.RawTerm)
rawTermLetVarAscription' =
  choice
    [ try $ do
        delimiter ":"
        (t, _) <- rawTerm
        return $ Just t,
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

rawTermNoeticVar :: Parser ((Hint, T.Text), C)
rawTermNoeticVar = do
  ((m, x), c) <- var
  return ((m, x), c)

rawTermEmbody :: Parser (RT.RawTerm, C)
rawTermEmbody = do
  m <- getCurrentHint
  delimiter "*"
  (e, c) <- rawTermBasic
  return (m :< RT.Embody e, c)

rawTermTau :: Parser (RT.RawTerm, C)
rawTermTau = do
  m <- getCurrentHint
  c <- keyword' "tau"
  return (m :< RT.Tau, c)

rawTermHole :: Parser (RT.RawTerm, C)
rawTermHole = do
  m <- getCurrentHint
  c <- keyword' "_"
  h <- lift $ Gensym.newPreHole m
  return (h, c)

parseDefInfo :: Hint -> Parser RT.DefInfo
parseDefInfo m = do
  functionVar <- var
  impArgs <- parseImplicitArgs
  expArgs <- argList preBinder
  codType <- parseDefInfoCod m
  e <- betweenBrace rawExpr
  return (fst functionVar, map f impArgs, map f expArgs, fst codType, fst e)

parseTopDefInfo :: Parser RT.TopDefInfo
parseTopDefInfo = do
  topDefHeader <- parseTopDefHeader
  (e, _) <- betweenBrace rawExpr
  return (topDefHeader, e)

parseTopDefHeader :: Parser RT.TopDefHeader
parseTopDefHeader = do
  m <- getCurrentHint
  funcBaseName <- baseName
  impDomArgList <- parseImplicitArgs
  expDomArgList <- argSeqOrList preBinder
  lift $ ensureArgumentLinearity S.empty $ map (\(mx, x, _) -> (mx, x)) expDomArgList
  codType <- parseDefInfoCod m
  return ((m, funcBaseName), map f impDomArgList, map f expDomArgList, fst codType)

parseDeclareItem :: (BN.BaseName -> App DD.DefiniteDescription) -> Parser RDE.RawDecl
parseDeclareItem nameLifter = do
  loc <- getCurrentHint
  name <- baseName >>= lift . nameLifter
  (isConstLike, impArgs, expArgs) <-
    choice
      [ do
          impArgs <- map f <$> parseImplicitArgs
          choice
            [ do
                expDomArgList <- argSeqOrList preBinder
                return (False, impArgs, map f expDomArgList),
              return (True, impArgs, [])
            ],
        do
          return (True, [], [])
      ]
  delimiter ":"
  cod <- fst <$> rawTerm
  return RDE.RawDecl {loc, name, isConstLike, impArgs, expArgs, cod}

parseImplicitArgs :: Parser [RawBinder (RT.RawTerm, C)]
parseImplicitArgs =
  choice
    [ parseImplicitArgs',
      return []
    ]

parseImplicitArgs' :: Parser [RawBinder (RT.RawTerm, C)]
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

parseDefInfoCod :: Hint -> Parser (RT.RawTerm, C)
parseDefInfoCod m =
  choice
    [ do
        delimiter ":"
        rawTerm,
      do
        h <- lift $ Gensym.newPreHole m
        return (h, [])
    ]

rawTermDefine :: Parser (RT.RawTerm, C)
rawTermDefine = do
  m <- getCurrentHint
  keyword "define"
  ((mFun, functionName), impArgs, expArgs, codType, e) <- parseDefInfo m
  return (m :< RT.PiIntro (LK.Fix (mFun, functionName, codType)) impArgs expArgs e, [])

rawTermMagic :: Parser (RT.RawTerm, C)
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

rawTermMagicBase :: T.Text -> Parser (C -> C -> a) -> Parser (a, C)
rawTermMagicBase k parser = do
  c1 <- keyword' k
  (c2, (magicF, c3)) <- betweenParen' parser
  return (magicF c1 c2, c3)

rawTermMagicCast :: Hint -> Parser (RT.RawTerm, C)
rawTermMagicCast m = do
  rawTermMagicBase "cast" $ do
    (castFrom, _) <- rawTerm
    (castTo, _) <- delimiter "," >> rawTerm
    (value, _) <- delimiter "," >> rawTerm
    return $ \_ _ -> m :< RT.Magic (M.Cast castFrom castTo value)

rawTermMagicStore :: Hint -> Parser (RT.RawTerm, C)
rawTermMagicStore m = do
  rawTermMagicBase "store" $ do
    (lt, _) <- lowType
    (value, _) <- delimiter "," >> rawTerm
    (pointer, _) <- delimiter "," >> rawTerm
    return $ \_ _ -> m :< RT.Magic (M.Store lt value pointer)

rawTermMagicLoad :: Hint -> Parser (RT.RawTerm, C)
rawTermMagicLoad m = do
  rawTermMagicBase "load" $ do
    (lt, _) <- lowType
    (pointer, _) <- delimiter "," >> rawTerm
    return $ \_ _ -> m :< RT.Magic (M.Load lt pointer)

rawTermMagicExternal :: Hint -> Parser (RT.RawTerm, C)
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
    let varArgAndTypeList' = map (second fst) varArgAndTypeList
    return $ \_ _ -> m :< RT.Magic (M.External domList cod extFunName (map fst es) varArgAndTypeList')

rawTermAndLowType :: Parser (LT.LowType, (RT.RawTerm, C))
rawTermAndLowType = do
  (e, c1) <- rawTerm
  (t, c2) <- lowType
  return (t, (e, c1 ++ c2))

rawTermMagicGlobal :: Hint -> Parser (RT.RawTerm, C)
rawTermMagicGlobal m = do
  rawTermMagicBase "global" $ do
    globalVarName <- string
    delimiter ","
    (lt, _) <- lowType
    return $ \_ _ -> m :< RT.Magic (M.Global lt (EN.ExternalName globalVarName))

lowType :: Parser (LT.LowType, C)
lowType = do
  choice
    [ lowTypePointer,
      lowTypeVoid,
      lowTypeNumber
    ]

lowTypePointer :: Parser (LT.LowType, C)
lowTypePointer = do
  c <- keyword' "pointer"
  return (LT.Pointer, c)

lowTypeVoid :: Parser (LT.LowType, C)
lowTypeVoid = do
  c <- keyword' "void"
  return (LT.Void, c)

lowTypeNumber :: Parser (LT.LowType, C)
lowTypeNumber = do
  (pt, c) <- primType
  return (LT.PrimNum pt, c)

primType :: Parser (PT.PrimType, C)
primType = do
  m <- getCurrentHint
  (sizeString, c) <- symbol'
  dataSize <- lift $ Env.getDataSize m
  case PT.fromText dataSize sizeString of
    Just primNum ->
      return (primNum, c)
    _ -> do
      failure (Just (asTokens sizeString)) (S.fromList [asLabel "i{n}", asLabel "f{n}"])

rawTermMatch :: Parser (RT.RawTerm, C)
rawTermMatch = do
  m <- getCurrentHint
  isNoetic <- choice [try (keyword "case") >> return True, keyword "match" >> return False]
  es <- commaList rawTermBasic
  (_, (patternRowList, c2)) <- betweenBrace' $ manyList $ rawTermPatternRow (length es)
  return (m :< RT.DataElim isNoetic (map fst es) (RP.new (map (second fst) patternRowList)), c2)

rawTermPatternRow :: Int -> Parser (RP.RawPatternRow (RT.RawTerm, C))
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
  rawTermPatternBasic

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
  return (m, RP.ListIntro patList)

parseName :: Parser ((Hint, Name), C)
parseName = do
  ((m, varText), c) <- var
  v <- interpretVarName m varText
  return (v, c)

rawTermPatternConsOrVar :: Parser (Hint, RP.RawPattern)
rawTermPatternConsOrVar = do
  ((m, varOrLocator), _) <- parseName
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

rawTermIf :: Parser (RT.RawTerm, C)
rawTermIf = do
  m <- getCurrentHint
  keyword "if"
  (ifCond, _) <- rawTerm
  (ifBody, _) <- betweenBrace rawExpr
  elseIfList <- many $ do
    keyword "else-if"
    (elseIfCond, _) <- rawTerm
    (elseIfBody, _) <- betweenBrace rawExpr
    return (elseIfCond, elseIfBody)
  keyword "else"
  (_, ((elseBody, _), c2)) <- betweenBrace' rawExpr
  return (m :< RT.If ifCond ifBody elseIfList elseBody, c2)

rawTermWhen :: Parser (RT.RawTerm, C)
rawTermWhen = do
  m <- getCurrentHint
  _ <- keyword' "when"
  (whenCond, _) <- rawTerm
  (_, ((whenBody, _), c2)) <- betweenBrace' rawExpr
  return (m :< RT.When whenCond whenBody, c2)

rawTermBrace :: Parser (RT.RawTerm, C)
rawTermBrace =
  betweenBrace rawExpr

rawTermWith :: Parser (RT.RawTerm, C)
rawTermWith = do
  m <- getCurrentHint
  keyword "with"
  (binder, _) <- rawTerm
  (_, ((body, _), c2)) <- betweenBrace' rawExpr
  return (m :< RT.With binder body, c2)

bind :: RawBinder RT.RawTerm -> RT.RawTerm -> RT.RawTerm -> RT.RawTerm
bind (m, x, t) e cont =
  m :< RT.Let RT.Plain (m, RP.Var (Var x), t) [] e cont

rawTermNoema :: Parser (RT.RawTerm, C)
rawTermNoema = do
  m <- getCurrentHint
  delimiter "&"
  (t, c) <- rawTermBasic
  return (m :< RT.Noema t, c)

rawTermFlowIntro :: Parser (RT.RawTerm, C)
rawTermFlowIntro = do
  m <- getCurrentHint
  keyword "detach"
  (_, ((e, _), c2)) <- betweenBrace' rawExpr
  return (m :< RT.Detach e, c2)

rawTermFlowElim :: Parser (RT.RawTerm, C)
rawTermFlowElim = do
  m <- getCurrentHint
  keyword "attach"
  (_, ((e, _), c2)) <- betweenBrace' rawExpr
  return (m :< RT.Attach e, c2)

rawTermOption :: Parser (RT.RawTerm, C)
rawTermOption = do
  m <- getCurrentHint
  c1 <- delimiter' "?"
  (t, c) <- rawTermBasic
  return (m :< RT.Option t, c1 ++ c)

rawTermAdmit :: Parser (RT.RawTerm, C)
rawTermAdmit = do
  m <- getCurrentHint
  c <- keyword' "admit"
  return (m :< RT.Admit, c)

rawTermAssert :: Parser (RT.RawTerm, C)
rawTermAssert = do
  m <- getCurrentHint
  keyword "assert"
  mText <- getCurrentHint
  message <- string
  (_, ((e, _), c2)) <- betweenBrace' rawExpr
  return (m :< RT.Assert (mText, message) e, c2)

rawTermPiElimOrSimple :: Parser (RT.RawTerm, C)
rawTermPiElimOrSimple = do
  m <- getCurrentHint
  ec@(e, _) <- rawTermSimple
  case e of
    _ :< RT.Var name -> do
      choice
        [ do
            keyword "of"
            (_, (rowList, c2)) <- betweenBrace' $ bulletListOrCommaSeq rawTermKeyValuePair
            return (m :< RT.PiElimByKey False name rowList, c2),
          rawTermPiElimCont False m ec
        ]
    _ -> do
      rawTermPiElimCont False m ec

rawTermPiElimCont :: IsExplicit -> Hint -> (RT.RawTerm, C) -> Parser (RT.RawTerm, C)
rawTermPiElimCont isExplicit m ec = do
  argListList <- many $ argList' rawExpr
  foldPiElim isExplicit m ec argListList

foldPiElim ::
  IsExplicit ->
  Hint ->
  (RT.RawTerm, C) ->
  [(C, ([(RT.RawTerm, C)], C))] ->
  Parser (RT.RawTerm, C)
foldPiElim isExplicit m (e, _) argListList =
  case argListList of
    [] ->
      return (e, [])
    (_, (args, c2)) : rest ->
      foldPiElim isExplicit m (m :< RT.PiElim isExplicit e (map fst args), c2) rest

preBinder :: Parser (RawBinder (RT.RawTerm, C))
preBinder =
  choice
    [ try preAscription,
      preAscription'
    ]

preAscription :: Parser (RawBinder (RT.RawTerm, C))
preAscription = do
  ((m, x), _) <- var
  delimiter ":"
  (a, c) <- rawTerm
  return (m, x, (a, c))

typeWithoutIdent :: Parser (RawBinder (RT.RawTerm, C))
typeWithoutIdent = do
  m <- getCurrentHint
  x <- lift Gensym.newTextForHole
  (t, c) <- rawTerm
  return (m, x, (t, c))

preAscription' :: Parser (RawBinder (RT.RawTerm, C))
preAscription' = do
  ((m, x), c) <- var
  h <- lift $ Gensym.newPreHole m
  return (m, x, (h, c))

rawTermListIntro :: Parser (RT.RawTerm, C)
rawTermListIntro = do
  m <- getCurrentHint
  (_, (es, c2)) <- betweenBracket' $ commaList rawExpr
  return (m :< RT.ListIntro (map fst es), c2)

rawTermPiElimExact :: Parser (RT.RawTerm, C)
rawTermPiElimExact = do
  m <- getCurrentHint
  keyword "exact"
  (e, c) <- rawTerm
  return (m :< RT.PiElimExact e, c)

rawTermPiElimExplicit :: Parser (RT.RawTerm, C)
rawTermPiElimExplicit = do
  m <- getCurrentHint
  keyword "call"
  ec@(e, _) <- rawTermSimple
  case e of
    _ :< RT.Var name -> do
      choice
        [ do
            keyword "of"
            rowList <- betweenBrace $ bulletListOrCommaSeq rawTermKeyValuePair
            return (m :< RT.PiElimByKey True name rowList, []),
          rawTermPiElimCont True m ec
        ]
    _ -> do
      rawTermPiElimCont True m ec

rawTermIntrospect :: Parser (RT.RawTerm, C)
rawTermIntrospect = do
  m <- getCurrentHint
  keyword "introspect"
  key <- symbol
  clauseList <- betweenBrace $ manyList rawTermIntrospectiveClause
  return (m :< RT.Introspect key (map (second fst) clauseList), [])

rawTermIntrospectiveClause :: Parser (Maybe T.Text, (RT.RawTerm, C))
rawTermIntrospectiveClause = do
  c <- symbol
  delimiter "=>"
  body <- rawExpr
  if c /= "default"
    then return (Just c, body)
    else return (Nothing, body)

rawTermSymbol :: Parser (RT.RawTerm, C)
rawTermSymbol = do
  ((m, varOrLocator), c) <- parseVarName
  return (m :< RT.Var varOrLocator, c)

parseVarName :: Parser ((Hint, Name), C)
parseVarName = do
  ((m, varText), c) <- var
  v <- interpretVarName m varText
  return (v, c)

interpretVarName :: Hint -> T.Text -> Parser (Hint, Name)
interpretVarName m varText = do
  case DD.getLocatorPair m varText of
    Left _ ->
      return (m, Var varText)
    Right (gl, ll) ->
      return (m, Locator (gl, ll))

rawTermTextIntro :: Parser (RT.RawTerm, C)
rawTermTextIntro = do
  m <- getCurrentHint
  (s, c) <- string'
  textType <- lift $ locatorToVarGlobal m coreText
  return (m :< RT.Prim (WP.Value (WPV.StaticText textType s)), c)

rawTermInteger :: Parser (RT.RawTerm, C)
rawTermInteger = do
  m <- getCurrentHint
  (intValue, c) <- try integer'
  h <- lift $ Gensym.newPreHole m
  return (m :< RT.Prim (WP.Value (WPV.Int h intValue)), c)

rawTermFloat :: Parser (RT.RawTerm, C)
rawTermFloat = do
  m <- getCurrentHint
  (floatValue, c) <- try float'
  h <- lift $ Gensym.newPreHole m
  return (m :< RT.Prim (WP.Value (WPV.Float h floatValue)), c)

preVar :: Hint -> T.Text -> RT.RawTerm
preVar m str =
  rawVar m (Var str)

locatorToVarGlobal :: Hint -> T.Text -> App RT.RawTerm
locatorToVarGlobal m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair (blur m) text
  return $ rawVar (blur m) (Locator (gl, ll))

rawVar :: Hint -> Name -> RT.RawTerm
rawVar m name =
  m :< RT.Var name

f :: RawBinder (a, C) -> RawBinder a
f (m, x, (t, _)) =
  (m, x, t)
