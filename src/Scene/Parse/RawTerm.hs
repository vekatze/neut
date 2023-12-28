module Scene.Parse.RawTerm
  ( rawExpr,
    preAscription,
    preBinder,
    parseDef,
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
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Trans
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.C
import Entity.Const
import Entity.DefiniteDescription qualified as DD
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.Key
import Entity.LowType qualified as LT
import Entity.Name
import Entity.PrimType qualified as PT
import Entity.PrimType.FromText qualified as PT
import Entity.RawBinder
import Entity.RawIdent
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.Syntax.Series qualified as SE
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
  e1 <- rawTerm
  choice
    [ do
        c1 <- delimiter ";"
        (e2, c2) <- rawExpr
        return (m :< RT.Seq e1 c1 e2, c2),
      return e1
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
  impArgs <- parseImplicitArgs
  expArgs <- seriesParen (choice [try preAscription, typeWithoutIdent])
  cArrow <- delimiter "->"
  (cod, c) <- rawTerm
  return (m :< RT.Pi impArgs expArgs cArrow cod, c)

rawTermPiIntro :: Parser (RT.RawTerm, C)
rawTermPiIntro = do
  m <- getCurrentHint
  impArgs <- parseImplicitArgs
  expArgs <- seriesParen preBinder
  cArrow <- delimiter "=>"
  (e, c) <- rawExpr
  return (m :< RT.PiIntro impArgs expArgs cArrow e, c)

rawTermPiOrConsOrAscOrBasic :: Parser (RT.RawTerm, C)
rawTermPiOrConsOrAscOrBasic = do
  m <- getCurrentHint
  (basic, cBasic) <- rawTermBasic
  choice
    [ do
        cArrow <- delimiter "->"
        x <- lift Gensym.newTextForHole
        (cod, c) <- rawTerm
        return
          ( m
              :< RT.Pi
                (SE.emptySeries SE.Angle SE.Comma, [])
                (SE.fromList SE.Paren SE.Comma [(m, x, [], [], basic)], cBasic)
                cArrow
                cod,
            c
          ),
      return (basic, cBasic)
    ]

rawTermKeyValuePair :: Parser ((Hint, Key, C, C, RT.RawTerm), C)
rawTermKeyValuePair = do
  ((m, key), c1) <- var
  choice
    [ do
        c2 <- delimiter "="
        (value, c) <- rawExpr
        return ((m, key, c1, c2, value), c),
      do
        return ((m, key, c1, [], m :< RT.Var (Var key)), [])
    ]

rawTermLet :: Hint -> Parser (RT.RawTerm, C)
rawTermLet mLet = do
  (letKind, c1) <-
    choice
      [ keyword "let" >>= \c1 -> return (RT.Plain, c1),
        keyword "try" >>= \c1 -> return (RT.Try, c1),
        keyword "bind" >>= \c1 -> return (RT.Bind, c1),
        keyword "tie" >>= \c1 -> return (RT.Noetic, c1)
      ]
  ((mx, patInner), c2) <- rawTermPattern
  (c3, (t, c4)) <- rawTermLetVarAscription mx
  noeticVarList <-
    choice
      [ do
          c <- keyword "on"
          vs <- bareSeries Nothing SE.Comma rawTermNoeticVar
          return $ SE.pushComment c vs,
        return $ SE.emptySeries' Nothing SE.Comma
      ]
  c5 <- delimiter "="
  lift $ ensureIdentLinearity S.empty $ SE.extract noeticVarList
  (e1, c6) <- rawExpr
  c7 <- delimiter "in"
  (e2, c) <- rawExpr
  return (mLet :< RT.Let letKind c1 (mx, patInner, c2, c3, t) c4 noeticVarList c5 e1 c6 c7 e2, c)

rawTermUse :: Hint -> Parser (RT.RawTerm, C)
rawTermUse m = do
  c1 <- keyword "use"
  (e, c2) <- rawTerm
  xs@(ys, _) <- seriesBrace preBinder
  c3 <- delimiter "in"
  lift $ ensureIdentLinearity S.empty $ map (\(_, (mx, x, _, _, _)) -> (mx, x)) $ SE.elems ys
  (cont, c) <- rawExpr
  return (m :< RT.Use c1 e c2 xs c3 cont, c)

rawTermLetVarAscription :: Hint -> Parser (C, (RT.RawTerm, C))
rawTermLetVarAscription m = do
  (c, mtc) <- rawTermLetVarAscription'
  case mtc of
    Just tc ->
      return (c, tc)
    Nothing -> do
      t <- lift $ Gensym.newPreHole m
      return (c, (t, []))

rawTermLetVarAscription' :: Parser (C, Maybe (RT.RawTerm, C))
rawTermLetVarAscription' =
  choice
    [ try $ do
        c <- delimiter ":"
        tc <- rawTerm
        return (c, Just tc),
      return ([], Nothing)
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
  c1 <- delimiter "*"
  (e, c) <- rawTermBasic
  return (m :< RT.Embody e, c1 ++ c)

rawTermTau :: Parser (RT.RawTerm, C)
rawTermTau = do
  m <- getCurrentHint
  c <- keyword "tau"
  return (m :< RT.Tau, c)

rawTermHole :: Parser (RT.RawTerm, C)
rawTermHole = do
  m <- getCurrentHint
  c <- keyword "_"
  h <- lift $ Gensym.newPreHole m
  return (h, c)

parseDef :: (BN.BaseName -> App a) -> Parser (RT.RawDef a, C)
parseDef nameLifter = do
  (topDefHeader, c1) <- parseDeclareItem nameLifter
  (c2, ((e, c3), c)) <- betweenBrace rawExpr
  return
    ( RT.RawDef
        { decl = topDefHeader,
          leadingComment = c1 ++ c2,
          body = e,
          trailingComment = c3
        },
      c
    )

parseDeclareItem :: (BN.BaseName -> App a) -> Parser (RT.RawDecl a, C)
parseDeclareItem nameLifter = do
  loc <- getCurrentHint
  (name, c1) <- baseName
  name' <- lift $ nameLifter name
  impArgs <- parseImplicitArgs
  (isConstLike, expArgs@(expSeries, _)) <- do
    choice
      [ do
          expDomArgList <- seqOrList preBinder
          return (False, expDomArgList),
        return (True, (SE.emptySeries SE.Paren SE.Comma, []))
      ]
  lift $ ensureArgumentLinearity S.empty $ map (\(mx, x, _, _, _) -> (mx, x)) $ SE.extract expSeries
  m <- getCurrentHint
  (c2, (cod, c)) <- parseDefInfoCod m
  return (RT.RawDecl {loc, name = (name', c1), isConstLike, impArgs, expArgs, cod = (c2, cod)}, c)

parseImplicitArgs :: Parser (SE.Series (RawBinder RT.RawTerm), C)
parseImplicitArgs =
  choice
    [ parseImplicitArgs',
      return (SE.emptySeries SE.Angle SE.Comma, [])
    ]

parseImplicitArgs' :: Parser (SE.Series (RawBinder RT.RawTerm), C)
parseImplicitArgs' =
  seriesAngle preBinder

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

parseDefInfoCod :: Hint -> Parser (C, (RT.RawTerm, C))
parseDefInfoCod m =
  choice
    [ do
        c <- delimiter ":"
        t <- rawTerm
        return (c, t),
      do
        h <- lift $ Gensym.newPreHole m
        return ([], (h, []))
    ]

rawTermDefine :: Parser (RT.RawTerm, C)
rawTermDefine = do
  m <- getCurrentHint
  c0 <- keyword "define"
  (defInfo, c) <- parseDef (return . BN.reify)
  return (m :< RT.PiIntroFix c0 defInfo, c)

rawTermMagic :: Parser (RT.RawTerm, C)
rawTermMagic = do
  m <- getCurrentHint
  c <- keyword "magic"
  choice
    [ rawTermMagicCast m c,
      rawTermMagicStore m c,
      rawTermMagicLoad m c,
      rawTermMagicExternal m c,
      rawTermMagicGlobal m c
    ]

rawTermMagicBase :: T.Text -> Parser (C -> C -> a) -> Parser (a, C)
rawTermMagicBase k parser = do
  c1 <- keyword k
  (c2, (magicF, c3)) <- betweenParen parser
  return (magicF c1 c2, c3)

rawTermMagicCast :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicCast m c = do
  rawTermMagicBase "cast" $ do
    castFrom <- rawExpr
    c3 <- delimiter ","
    castTo <- rawExpr
    c4 <- delimiter ","
    value <- rawExpr
    return $ \c1 c2 -> m :< RT.Magic c (RT.Cast c1 (c2, castFrom) (c3, castTo) (c4, value))

rawTermMagicStore :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicStore m c = do
  rawTermMagicBase "store" $ do
    lt <- lowType
    c3 <- delimiter ","
    value <- rawExpr
    c4 <- delimiter ","
    pointer <- rawExpr
    return $ \c1 c2 -> m :< RT.Magic c (RT.Store c1 (c2, lt) (c3, value) (c4, pointer))

rawTermMagicLoad :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicLoad m c = do
  rawTermMagicBase "load" $ do
    lt <- lowType
    c3 <- delimiter ","
    pointer <- rawExpr
    return $ \c1 c2 -> m :< RT.Magic c (RT.Load c1 (c2, lt) (c3, pointer))

rawTermMagicExternal :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicExternal m c0 = do
  c1 <- keyword "external"
  (extFunName, cExt) <- symbol
  let extFunName' = EN.ExternalName extFunName
  (es, c2) <- seriesParen rawExpr
  choice
    [ do
        (s, c) <- seriesParen rawExprAndLowType
        return (m :< RT.Magic c0 (RT.External c1 extFunName' cExt es (Just (c2, s))), c),
      return (m :< RT.Magic c0 (RT.External c1 extFunName' cExt es Nothing), c2)
    ]

rawExprAndLowType :: Parser (RT.VarArg, C)
rawExprAndLowType = do
  m <- getCurrentHint
  (e, c1) <- rawExpr
  c2 <- delimiter ":"
  (t, c) <- lowType
  return ((m, e, c1, c2, t), c)

rawTermMagicGlobal :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGlobal m c = do
  rawTermMagicBase "global" $ do
    (globalVarName, c3) <- string
    c4 <- delimiter ","
    lt <- lowType
    return $ \c1 c2 -> m :< RT.Magic c (RT.Global c1 (c2, (EN.ExternalName globalVarName, c3)) (c4, lt))

lowType :: Parser (LT.LowType, C)
lowType = do
  choice
    [ lowTypePointer,
      lowTypeVoid,
      lowTypeNumber
    ]

lowTypePointer :: Parser (LT.LowType, C)
lowTypePointer = do
  c <- keyword "pointer"
  return (LT.Pointer, c)

lowTypeVoid :: Parser (LT.LowType, C)
lowTypeVoid = do
  c <- keyword "void"
  return (LT.Void, c)

lowTypeNumber :: Parser (LT.LowType, C)
lowTypeNumber = do
  (pt, c) <- primType
  return (LT.PrimNum pt, c)

primType :: Parser (PT.PrimType, C)
primType = do
  m <- getCurrentHint
  (sizeString, c) <- symbol
  dataSize <- lift $ Env.getDataSize m
  case PT.fromText dataSize sizeString of
    Just primNum ->
      return (primNum, c)
    _ -> do
      failure (Just (asTokens sizeString)) (S.fromList [asLabel "i{n}", asLabel "f{n}"])

rawTermMatch :: Parser (RT.RawTerm, C)
rawTermMatch = do
  m <- getCurrentHint
  (c1, isNoetic) <-
    choice
      [ do
          c1 <- try (keyword "case")
          return (c1, True),
        do
          c1 <- keyword "match"
          return (c1, False)
      ]
  es <- bareSeries Nothing SE.Comma rawTermBasic
  (patternRowList, c) <- seriesBraceList $ rawTermPatternRow (length $ SE.extract es)
  return (m :< RT.DataElim c1 isNoetic es patternRowList, c)

rawTermPatternRow :: Int -> Parser (RP.RawPatternRow RT.RawTerm, C)
rawTermPatternRow patternSize = do
  m <- getCurrentHint
  patternList <- bareSeries Nothing SE.Comma rawTermPattern
  let len = length $ SE.extract patternList
  unless (len == patternSize) $ do
    lift $
      Throw.raiseError m $
        "the size of the pattern row `"
          <> T.pack (show len)
          <> "` doesn't match with its input size `"
          <> T.pack (show patternSize)
          <> "`"
  cArrow <- delimiter "=>"
  (body, c) <- rawExpr
  return ((patternList, cArrow, body), c)

rawTermPattern :: Parser ((Hint, RP.RawPattern), C)
rawTermPattern = do
  rawTermPatternBasic

rawTermPatternBasic :: Parser ((Hint, RP.RawPattern), C)
rawTermPatternBasic =
  choice
    [ rawTermPatternListIntro,
      rawTermPatternConsOrVar
    ]

rawTermPatternListIntro :: Parser ((Hint, RP.RawPattern), C)
rawTermPatternListIntro = do
  m <- getCurrentHint
  (patList, c) <- seriesBracket rawTermPattern
  return ((m, RP.ListIntro patList), c)

parseName :: Parser ((Hint, Name), C)
parseName = do
  ((m, varText), c) <- var
  v <- interpretVarName m varText
  return (v, c)

rawTermPatternConsOrVar :: Parser ((Hint, RP.RawPattern), C)
rawTermPatternConsOrVar = do
  ((m, varOrLocator), c1) <- parseName
  choice
    [ do
        (patArgs, c) <- seriesParen rawTermPattern
        return ((m, RP.Cons varOrLocator c1 (RP.Paren patArgs)), c),
      do
        (kvs, c) <- keyValueArgs rawTermPatternKeyValuePair
        return ((m, RP.Cons varOrLocator c1 (RP.Of kvs)), c),
      do
        return ((m, RP.Var varOrLocator), c1)
    ]

rawTermPatternKeyValuePair :: Parser ((Key, (Hint, C, RP.RawPattern)), C)
rawTermPatternKeyValuePair = do
  mFrom <- getCurrentHint
  (from, c1) <- symbol
  choice
    [ do
        c2 <- delimiter "="
        ((mTo, to), c) <- rawTermPattern
        return ((from, (mTo, c1 ++ c2, to)), c),
      do
        return ((from, (mFrom, [], RP.Var (Var from))), []) -- record rhyming
    ]

rawTermIf :: Parser (RT.RawTerm, C)
rawTermIf = do
  m <- getCurrentHint
  (ifClause, c1) <- rawTermIfClause "if"
  (elseIfClauseList, c2) <- fmap joinComment $ many $ rawTermIfClause "else-if"
  c3 <- keyword "else"
  (c4, (elseBody, c)) <- betweenBrace rawExpr
  let (elseIfClauseList', elseBody') = adjustComment c1 elseIfClauseList (c2 ++ c3 ++ c4, elseBody)
  return (m :< RT.If ifClause elseIfClauseList' elseBody', c)

adjustComment :: C -> [RT.IfClause a] -> RT.EL a -> ([RT.IfClause a], RT.EL a)
adjustComment c elseIfClauseList (c', x) = do
  case elseIfClauseList of
    [] ->
      ([], (c ++ c', x))
    clause : rest -> do
      (RT.pushCommentToIfClause c clause : rest, (c', x))

joinComment :: [(RT.IfClause a, C)] -> ([RT.IfClause a], C)
joinComment xs =
  case xs of
    [] ->
      ([], [])
    (clause, c) : rest -> do
      let (y, trail) = joinComment rest
      case y of
        [] ->
          ([clause], c ++ trail)
        clause' : clauseList ->
          (clause : RT.pushCommentToIfClause c clause' : clauseList, trail)

rawTermIfClause :: T.Text -> Parser (RT.IfClause RT.RawTerm, C)
rawTermIfClause k = do
  c1 <- keyword k
  cond <- rawTerm
  (c2, (body, c3)) <- betweenBrace rawExpr
  return (((c1, cond), (c2, body)), c3)

rawTermWhen :: Parser (RT.RawTerm, C)
rawTermWhen = do
  m <- getCurrentHint
  (whenClause, c) <- rawTermIfClause "when"
  return (m :< RT.When whenClause, c)

rawTermBrace :: Parser (RT.RawTerm, C)
rawTermBrace = do
  m <- getCurrentHint
  (c1, (e, c)) <- betweenBrace rawExpr
  return (m :< RT.Brace c1 e, c)

rawTermWith :: Parser (RT.RawTerm, C)
rawTermWith = do
  m <- getCurrentHint
  c1 <- keyword "with"
  (binder, c2) <- rawTerm
  (c3, (body, c)) <- betweenBrace rawExpr
  return (m :< RT.With c1 binder c2 c3 body, c)

rawTermNoema :: Parser (RT.RawTerm, C)
rawTermNoema = do
  m <- getCurrentHint
  c1 <- delimiter "&"
  (t, c) <- rawTermBasic
  return (m :< RT.Noema t, c1 ++ c)

rawTermFlowIntro :: Parser (RT.RawTerm, C)
rawTermFlowIntro = do
  m <- getCurrentHint
  c1 <- keyword "detach"
  (c2, (e, c)) <- betweenBrace rawExpr
  return (m :< RT.Detach c1 c2 e, c)

rawTermFlowElim :: Parser (RT.RawTerm, C)
rawTermFlowElim = do
  m <- getCurrentHint
  c1 <- keyword "attach"
  (c2, (e, c)) <- betweenBrace rawExpr
  return (m :< RT.Attach c1 c2 e, c)

rawTermOption :: Parser (RT.RawTerm, C)
rawTermOption = do
  m <- getCurrentHint
  c1 <- delimiter "?"
  (t, c) <- rawTermBasic
  return (m :< RT.Option t, c1 ++ c)

rawTermAdmit :: Parser (RT.RawTerm, C)
rawTermAdmit = do
  m <- getCurrentHint
  c <- keyword "admit"
  return (m :< RT.Admit, c)

rawTermAssert :: Parser (RT.RawTerm, C)
rawTermAssert = do
  m <- getCurrentHint
  c1 <- keyword "assert"
  mText <- getCurrentHint
  (message, c2) <- string
  (c3, (e, c)) <- betweenBrace rawExpr
  return (m :< RT.Assert c1 (mText, message) c2 c3 e, c)

rawTermPiElimOrSimple :: Parser (RT.RawTerm, C)
rawTermPiElimOrSimple = do
  m <- getCurrentHint
  ec@(e, c1) <- rawTermSimple
  case e of
    _ :< RT.Var name -> do
      choice
        [ do
            (kvs, c) <- keyValueArgs rawTermKeyValuePair
            return (m :< RT.PiElimByKey name c1 kvs, c),
          rawTermPiElimCont m ec
        ]
    _ -> do
      rawTermPiElimCont m ec

keyValueArgs :: Parser (a, C) -> Parser (SE.Series a, C)
keyValueArgs p = do
  c1 <- keyword "of"
  choice
    [ try $ do
        (kvs, c) <- series (Just ("of", c1)) SE.Brace SE.Hyphen p
        if SE.isEmpty kvs
          then failure Nothing (S.fromList [asLabel "bullet list"])
          else return (kvs, c),
      do
        series (Just ("of", c1)) SE.Brace SE.Comma p
    ]

rawTermPiElimCont :: Hint -> (RT.RawTerm, C) -> Parser (RT.RawTerm, C)
rawTermPiElimCont m ec = do
  argListList <- many $ seriesParen rawExpr
  return $ foldPiElim m ec argListList

foldPiElim ::
  Hint ->
  (RT.RawTerm, C) ->
  [(SE.Series RT.RawTerm, C)] ->
  (RT.RawTerm, C)
foldPiElim m (e, c) argListList =
  case argListList of
    [] ->
      (e, c)
    (args, c1) : rest ->
      foldPiElim m (m :< RT.PiElim e c args, c1) rest

preBinder :: Parser (RawBinder RT.RawTerm, C)
preBinder =
  choice
    [ try preAscription,
      preAscription'
    ]

preAscription :: Parser (RawBinder RT.RawTerm, C)
preAscription = do
  ((m, x), c1) <- var
  c2 <- delimiter ":"
  (a, c) <- rawTerm
  return ((m, x, c1, c2, a), c)

typeWithoutIdent :: Parser (RawBinder RT.RawTerm, C)
typeWithoutIdent = do
  m <- getCurrentHint
  x <- lift Gensym.newTextForHole
  (t, c) <- rawTerm
  return ((m, x, [], [], t), c)

preAscription' :: Parser (RawBinder RT.RawTerm, C)
preAscription' = do
  ((m, x), c) <- var
  h <- lift $ Gensym.newPreHole m
  return ((m, x, c, [], h), [])

rawTermListIntro :: Parser (RT.RawTerm, C)
rawTermListIntro = do
  m <- getCurrentHint
  (es, c) <- seriesBracket rawExpr
  return (m :< RT.ListIntro es, c)

rawTermPiElimExact :: Parser (RT.RawTerm, C)
rawTermPiElimExact = do
  m <- getCurrentHint
  c1 <- keyword "exact"
  (e, c) <- rawTerm
  return (m :< RT.PiElimExact c1 e, c)

rawTermIntrospect :: Parser (RT.RawTerm, C)
rawTermIntrospect = do
  m <- getCurrentHint
  c1 <- keyword "introspect"
  (key, c2) <- symbol
  (clauseList, c) <- seriesBraceList rawTermIntrospectiveClause
  return (m :< RT.Introspect c1 key c2 clauseList, c)

rawTermIntrospectiveClause :: Parser ((Maybe T.Text, C, RT.RawTerm), C)
rawTermIntrospectiveClause = do
  (s, cKey) <- symbol
  cArrow <- delimiter "=>"
  (body, c) <- rawExpr
  if s /= "default"
    then return ((Just s, cKey ++ cArrow, body), c)
    else return ((Nothing, cKey ++ cArrow, body), c)

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
  (s, c) <- string
  textType <- lift $ locatorToVarGlobal m coreText
  return (m :< RT.Prim (WP.Value (WPV.StaticText textType s)), c)

rawTermInteger :: Parser (RT.RawTerm, C)
rawTermInteger = do
  m <- getCurrentHint
  (intValue, c) <- try integer
  h <- lift $ Gensym.newPreHole m
  return (m :< RT.Prim (WP.Value (WPV.Int h intValue)), c)

rawTermFloat :: Parser (RT.RawTerm, C)
rawTermFloat = do
  m <- getCurrentHint
  (floatValue, c) <- try float
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
f (m, x, c1, c2, (t, _)) =
  (m, x, c1, c2, t)
