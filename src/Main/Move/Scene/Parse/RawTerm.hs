module Main.Move.Scene.Parse.RawTerm
  ( rawExpr,
    rawTerm,
    var,
    preAscription,
    preBinder,
    parseDef,
    parseGeist,
    parseDefInfoCod,
    typeWithoutIdent,
    parseImplicitArgs,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.Trans
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Common.Move.CreateSymbol (newTextForHole)
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.Hint
import Language.Common.Rule.Rune qualified as RU
import Language.RawTerm.Move.CreateHole qualified as RT
import Language.RawTerm.Rule.C
import Language.RawTerm.Rule.Key
import Language.RawTerm.Rule.Name
import Language.RawTerm.Rule.NecessityVariant (NecessityVariant (..), showNecessityVariant)
import Language.RawTerm.Rule.RawBinder
import Language.RawTerm.Rule.RawIdent
import Language.RawTerm.Rule.RawPattern qualified as RP
import Language.RawTerm.Rule.RawTerm qualified as RT
import Language.RawTerm.Rule.Syntax.Series qualified as SE
import Main.Move.Context.EIO (EIO, raiseError)
import Main.Move.Scene.Parse.Core
import Main.Rule.Const
import Text.Megaparsec
import Text.Read qualified as R

rawExpr :: Handle -> Parser (RT.RawTerm, C)
rawExpr h = do
  m <- getCurrentHint
  choice
    [ rawTermLet h m,
      rawTermBoxElim h m,
      rawTermUse h m,
      rawTermPin h m,
      do
        e1 <- rawTerm h
        choice
          [ do
              c1 <- delimiter ";"
              (e2, c2) <- rawExpr h
              return (m :< RT.Seq e1 c1 e2, c2),
            return e1
          ]
    ]

rawTerm :: Handle -> Parser (RT.RawTerm, C)
rawTerm h = do
  choice
    [ rawTermDefine h,
      rawTermIntrospect h,
      rawTermIncludeText,
      rawTermMagic h,
      rawTermMatch h,
      rawTermPi h,
      rawTermPiIntro h,
      rawTermBox h,
      rawTermBoxNoema h,
      rawTermBoxIntro h,
      rawTermBoxIntroQuote h,
      rawTermIf h,
      rawTermWhen h,
      rawTermAssert h,
      rawTermOption h,
      rawTermFlowIntro h,
      rawTermFlowElim h,
      rawTermEmbody h,
      rawTermWith h,
      rawTermPiElimExact h,
      do
        m <- getCurrentHint
        ec@(e, c1) <- rawTermSimple h
        case e of
          _ :< RT.Var name -> do
            choice
              [ do
                  (kvs, c) <- keyValueArgs $ rawTermKeyValuePair h
                  return (m :< RT.PiElimByKey name c1 kvs, c),
                rawTermPiElimCont h ec
              ]
          _ -> do
            rawTermPiElimCont h ec
    ]

rawTermProjection :: Handle -> Parser ([((Hint, T.Text), Loc)], C)
rawTermProjection h = do
  choice
    [ do
        c1 <- delimiter "::"
        (mv, c) <- var h
        loc <- getCurrentLoc
        (mvs, c') <- rawTermProjection h
        return ((mv, loc) : mvs, c1 ++ c ++ c'),
      do
        return ([], [])
    ]

rawTermPiElimCont :: Handle -> (RT.RawTerm, C) -> Parser (RT.RawTerm, C)
rawTermPiElimCont h (e, c) = do
  (projections, cProj) <- rawTermProjection h
  let e'@(m :< _) = foldProjection e projections
  argListList <- many $ seriesParen (rawExpr h)
  return $ foldPiElim m (e', c ++ cProj) argListList

foldProjection :: RT.RawTerm -> [((Hint, T.Text), Loc)] -> RT.RawTerm
foldProjection e projections =
  case projections of
    [] ->
      e
    (proj@(mProj, _), loc) : rest ->
      foldProjection (mProj :< RT.Projection e proj loc) rest

{-# INLINE rawTermSimple #-}
rawTermSimple :: Handle -> Parser (RT.RawTerm, C)
rawTermSimple h = do
  choice
    [ rawTermBrace h,
      rawTermListIntro h,
      rawTermTextIntro,
      rawTermRune,
      rawTermRuneIntro,
      rawTermTau,
      rawTermPointer,
      rawTermVoid,
      rawTermAdmit,
      rawTermHole h,
      rawTermSymbol h
    ]

rawTermPi :: Handle -> Parser (RT.RawTerm, C)
rawTermPi h = do
  m <- getCurrentHint
  impArgs <- parseImplicitArgs h
  expArgs <- seriesParen (choice [try $ var h >>= preAscription h, typeWithoutIdent h])
  cArrow <- delimiter "->"
  (cod, c) <- rawTerm h
  loc <- getCurrentLoc
  return (m :< RT.Pi impArgs expArgs cArrow cod loc, c)

rawTermPiIntro :: Handle -> Parser (RT.RawTerm, C)
rawTermPiIntro h = do
  m <- getCurrentHint
  c0 <- keyword "function"
  (defInfo, c) <- parseDef h $ do
    return ((), [])
  return (m :< RT.PiIntro c0 defInfo, c)

rawTermKeyValuePair :: Handle -> Parser ((Hint, Key, C, C, RT.RawTerm), C)
rawTermKeyValuePair h = do
  ((m, key), c1) <- var h
  choice
    [ do
        c2 <- delimiter "="
        (value, c) <- rawTerm h
        return ((m, key, c1, c2, value), c),
      do
        return ((m, key, c1, [], m :< RT.Var (Var key)), [])
    ]

{-# INLINE rawTermLetKeyword #-}
rawTermLetKeyword :: RT.LetKind -> Parser (RT.LetKind, C)
rawTermLetKeyword letKind = do
  keyword (RT.decodeLetKind letKind) >>= \c1 -> return (letKind, c1)

rawTermLet :: Handle -> Hint -> Parser (RT.RawTerm, C)
rawTermLet h mLet = do
  (letKind, c1) <-
    choice
      [ rawTermLetKeyword (RT.Plain False),
        rawTermLetKeyword RT.Try,
        rawTermLetKeyword RT.Bind,
        rawTermLetKeyword RT.Noetic
      ]
  ((mx, patInner), c2) <- rawTermPattern h
  (c3, (t, c4)) <- rawTermLetVarAscription h mx
  noeticVarList <-
    choice
      [ do
          c <- keyword "on"
          vs <- bareSeries Nothing SE.Comma $ rawTermNoeticVar h
          return $ SE.pushComment c vs,
        return $ SE.emptySeries' Nothing SE.Comma
      ]
  c5 <- delimiter "="
  lift $ ensureIdentLinearity S.empty $ SE.extract noeticVarList
  (e1, c6) <- rawExpr h
  loc <- getCurrentLoc
  c7 <- delimiter "in"
  (e2, c) <- rawExpr h
  endLoc <- getCurrentLoc
  case (letKind, SE.isEmpty noeticVarList) of
    (RT.Plain _, False) -> do
      return (mLet :< RT.LetOn False c1 (mx, patInner, c2, c3, t) c4 noeticVarList c5 e1 c6 loc c7 e2 endLoc, c)
    (_, False) ->
      lift $ raiseError mLet $ "`on` cannot be used with: `" <> RT.decodeLetKind letKind <> "`"
    _ ->
      return (mLet :< RT.Let letKind c1 (mx, patInner, c2, c3, t) c4 c5 e1 c6 loc c7 e2 endLoc, c)

rawTermBoxElim :: Handle -> Hint -> Parser (RT.RawTerm, C)
rawTermBoxElim h mLet = do
  let keywordReader nv = keyword (showNecessityVariant nv) >>= \c1 -> return (nv, c1)
  (nv, c1) <-
    choice
      [ keywordReader VariantK,
        keywordReader VariantT
      ]
  ((mx, patInner), c2) <- rawTermPattern h
  (c3, (t, c4)) <- rawTermLetVarAscription h mx
  noeticVarList <-
    choice
      [ do
          c <- keyword "on"
          vs <- bareSeries Nothing SE.Comma $ rawTermNoeticVar h
          return $ SE.pushComment c vs,
        return $ SE.emptySeries' Nothing SE.Comma
      ]
  c5 <- delimiter "="
  lift $ ensureIdentLinearity S.empty $ SE.extract noeticVarList
  (e1, c6) <- rawExpr h
  loc <- getCurrentLoc
  c7 <- delimiter "in"
  (e2, c) <- rawExpr h
  endLoc <- getCurrentLoc
  return (mLet :< RT.BoxElim nv False c1 (mx, patInner, c2, c3, t) c4 noeticVarList c5 e1 c6 loc c7 e2 endLoc, c)

rawTermPin :: Handle -> Hint -> Parser (RT.RawTerm, C)
rawTermPin h m = do
  c1 <- keyword "pin"
  ((mx, x), c2) <- rawTermNoeticVar h
  (c3, (t, c4)) <- rawTermLetVarAscription h mx
  noeticVarList <-
    choice
      [ do
          c <- keyword "on"
          vs <- bareSeries Nothing SE.Comma $ rawTermNoeticVar h
          return $ SE.pushComment c vs,
        return $ SE.emptySeries' Nothing SE.Comma
      ]
  c5 <- delimiter "="
  (e1, c6) <- rawExpr h
  loc <- getCurrentLoc
  c7 <- delimiter "in"
  (e2, c) <- rawExpr h
  endLoc <- getCurrentLoc
  return (m :< RT.Pin c1 (mx, x, c2, c3, t) c4 noeticVarList c5 e1 c6 loc c7 e2 endLoc, c)

rawTermUse :: Handle -> Hint -> Parser (RT.RawTerm, C)
rawTermUse h m = do
  c1 <- keyword "use"
  (e, c2) <- rawTerm h
  xs@(ys, _) <- seriesBrace $ preBinder h
  c3 <- delimiter "in"
  lift $ ensureIdentLinearity S.empty $ map (\(_, (mx, x, _, _, _)) -> (mx, x)) $ SE.elems ys
  (cont, c) <- rawExpr h
  loc <- getCurrentLoc
  return (m :< RT.Use c1 e c2 xs c3 cont loc, c)

rawTermLetVarAscription :: Handle -> Hint -> Parser (C, (RT.RawTerm, C))
rawTermLetVarAscription h m = do
  (c, mtc) <- rawTermLetVarAscription' h
  case mtc of
    Just tc ->
      return (c, tc)
    Nothing -> do
      t <- liftIO $ RT.createHole (gensymHandle h) m
      return (c, (t, []))

rawTermLetVarAscription' :: Handle -> Parser (C, Maybe (RT.RawTerm, C))
rawTermLetVarAscription' h =
  choice
    [ do
        c <- delimiter ":"
        tc <- rawTerm h
        return (c, Just tc),
      return ([], Nothing)
    ]

ensureIdentLinearity :: S.Set RawIdent -> [(Hint, RawIdent)] -> EIO ()
ensureIdentLinearity foundVarSet vs =
  case vs of
    [] ->
      return ()
    (m, name) : rest
      | S.member name foundVarSet ->
          raiseError m $ "Found a non-linear occurrence of `" <> name <> "`."
      | otherwise ->
          ensureIdentLinearity (S.insert name foundVarSet) rest

rawTermNoeticVar :: Handle -> Parser ((Hint, T.Text), C)
rawTermNoeticVar h = do
  ((m, x), c) <- var h
  return ((m, x), c)

rawTermEmbody :: Handle -> Parser (RT.RawTerm, C)
rawTermEmbody h = do
  m <- getCurrentHint
  c1 <- delimiter "*"
  (e, c) <- rawTerm h
  return (m :< RT.Embody e, c1 ++ c)

rawTermTau :: Parser (RT.RawTerm, C)
rawTermTau = do
  m <- getCurrentHint
  c <- keyword "type"
  return (m :< RT.Tau, c)

rawTermPointer :: Parser (RT.RawTerm, C)
rawTermPointer = do
  m <- getCurrentHint
  c <- keyword "pointer"
  return (m :< RT.Pointer, c)

rawTermVoid :: Parser (RT.RawTerm, C)
rawTermVoid = do
  m <- getCurrentHint
  c <- keyword "void"
  return (m :< RT.Void, c)

rawTermHole :: Handle -> Parser (RT.RawTerm, C)
rawTermHole h = do
  m <- getCurrentHint
  c <- keyword "_"
  hole <- liftIO $ RT.createHole (gensymHandle h) m
  return (hole, c)

parseDef :: Handle -> Parser (a, C) -> Parser (RT.RawDef a, C)
parseDef h nameParser = do
  (geist, c1) <- parseGeist h nameParser
  (c2, ((e, c3), loc, c)) <- betweenBrace' $ rawExpr h
  return
    ( RT.RawDef
        { geist,
          leadingComment = c1 ++ c2,
          body = e,
          trailingComment = c3,
          endLoc = loc
        },
      c
    )

parseGeist :: Handle -> Parser (a, C) -> Parser (RT.RawGeist a, C)
parseGeist h nameParser = do
  loc <- getCurrentHint
  (name', c1) <- nameParser
  impArgs <- parseImplicitArgs h
  (isConstLike, expArgs@(expSeries, _)) <- do
    choice
      [ do
          expDomArgList <- seriesParen $ preBinder h
          return (False, expDomArgList),
        return (True, (SE.emptySeries (Just SE.Paren) SE.Comma, []))
      ]
  lift $ ensureArgumentLinearity S.empty $ map (\(mx, x, _, _, _) -> (mx, x)) $ SE.extract expSeries
  m <- getCurrentHint
  (c2, (cod, c)) <- parseDefInfoCod h m
  return (RT.RawGeist {loc, name = (name', c1), isConstLike, impArgs, expArgs, cod = (c2, cod)}, c)

parseImplicitArgs :: Handle -> Parser (SE.Series (RawBinder RT.RawTerm), C)
parseImplicitArgs h =
  choice
    [ parseImplicitArgs' h,
      return (SE.emptySeries (Just SE.Angle) SE.Comma, [])
    ]

parseImplicitArgs' :: Handle -> Parser (SE.Series (RawBinder RT.RawTerm), C)
parseImplicitArgs' h =
  seriesAngle $ preBinder h

ensureArgumentLinearity :: S.Set RawIdent -> [(Hint, RawIdent)] -> EIO ()
ensureArgumentLinearity foundVarSet vs =
  case vs of
    [] ->
      return ()
    (m, name) : rest
      | S.member name foundVarSet ->
          raiseError m $ "Found a non-linear occurrence of `" <> name <> "`."
      | otherwise ->
          ensureArgumentLinearity (S.insert name foundVarSet) rest

parseDefInfoCod :: Handle -> Hint -> Parser (C, (RT.RawTerm, C))
parseDefInfoCod h m =
  choice
    [ do
        c <- delimiter ":"
        t <- rawTerm h
        return (c, t),
      do
        hole <- liftIO $ RT.createHole (gensymHandle h) m
        return ([], (hole, []))
    ]

rawTermDefine :: Handle -> Parser (RT.RawTerm, C)
rawTermDefine h = do
  m <- getCurrentHint
  c0 <- keyword "define"
  (defInfo, c) <- parseDef h $ do
    (name, c1) <- baseName
    name' <- liftIO $ adjustHoleVar h name
    return (name', c1)
  return (m :< RT.PiIntroFix c0 defInfo, c)

adjustHoleVar :: Handle -> BN.BaseName -> IO T.Text
adjustHoleVar h bn = do
  let bn' = BN.reify bn
  if bn' /= "_"
    then return bn'
    else newTextForHole (gensymHandle h)

rawTermMagic :: Handle -> Parser (RT.RawTerm, C)
rawTermMagic h = do
  m <- getCurrentHint
  c <- keyword "magic"
  choice
    [ rawTermMagicCast h m c,
      rawTermMagicStore h m c,
      rawTermMagicLoad h m c,
      rawTermMagicAlloca h m c,
      rawTermMagicExternal h m c,
      rawTermMagicOpaqueValue h m c,
      rawTermMagicGlobal h m c
    ]

rawTermMagicBase :: T.Text -> Parser (C -> C -> a) -> Parser (a, C)
rawTermMagicBase k parser = do
  c1 <- keyword k
  (c2, (magicF, c3)) <- betweenParen parser
  return (magicF c1 c2, c3)

rawTermMagicCast :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicCast h m c = do
  rawTermMagicBase "cast" $ do
    castFrom <- rawExpr h
    c3 <- delimiter ","
    castTo <- rawExpr h
    c4 <- delimiter ","
    value <- rawExpr h
    c6 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Cast c1 (c2, castFrom) (c3, castTo) (c4, value) c6)

rawTermMagicStore :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicStore h m c = do
  rawTermMagicBase "store" $ do
    t <- rawExpr h
    c3 <- delimiter ","
    value <- rawExpr h
    c4 <- delimiter ","
    pointer <- rawExpr h
    c5 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Store c1 (c2, t) (c3, value) (c4, pointer) c5)

rawTermMagicLoad :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicLoad h m c = do
  rawTermMagicBase "load" $ do
    t <- rawExpr h
    c3 <- delimiter ","
    pointer <- rawExpr h
    c4 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Load c1 (c2, t) (c3, pointer) c4)

rawTermMagicAlloca :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicAlloca h m c = do
  rawTermMagicBase "alloca" $ do
    t <- rawExpr h
    c3 <- delimiter ","
    size <- rawExpr h
    c4 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Alloca c1 (c2, t) (c3, size) c4)

rawTermMagicExternal :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicExternal h m c0 = do
  c1 <- keyword "external"
  mUse <- getCurrentHint
  (extFunName, cExt) <- symbol
  let extFunName' = EN.ExternalName extFunName
  (es, c2) <- seriesParen (rawExpr h)
  choice
    [ do
        (s, c) <- seriesParen (rawExprAndLowType h)
        return (m :< RT.Magic c0 (RT.External c1 mUse extFunName' cExt es (Just (c2, s))), c),
      return (m :< RT.Magic c0 (RT.External c1 mUse extFunName' cExt es Nothing), c2)
    ]

rawExprAndLowType :: Handle -> Parser (RT.VarArg, C)
rawExprAndLowType h = do
  m <- getCurrentHint
  (e, c1) <- rawExpr h
  c2 <- delimiter ":"
  (t, c) <- rawTerm h
  return ((m, e, c1, c2, t), c)

rawTermMagicGlobal :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGlobal h m c = do
  rawTermMagicBase "global" $ do
    (globalVarName, c3) <- string
    c4 <- delimiter ","
    lt <- rawExpr h
    c5 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Global c1 (c2, (EN.ExternalName globalVarName, c3)) (c4, lt) c5)

rawTermMagicOpaqueValue :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicOpaqueValue h m c0 = do
  c1 <- keyword "opaque-value"
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.Magic c0 (RT.OpaqueValue c1 (c2, e)), c)

rawTermMatch :: Handle -> Parser (RT.RawTerm, C)
rawTermMatch h = do
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
  es <- bareSeries Nothing SE.Comma $ rawTerm h
  (patternRowList, c) <- seriesBraceList $ rawTermPatternRow h (length $ SE.extract es)
  return (m :< RT.DataElim c1 isNoetic es patternRowList, c)

rawTermPatternRow :: Handle -> Int -> Parser (RP.RawPatternRow RT.RawTerm, C)
rawTermPatternRow h patternSize = do
  m <- getCurrentHint
  patternList <- bareSeries Nothing SE.Comma $ rawTermPattern h
  if SE.isEmpty patternList
    then failure Nothing (S.fromList [asLabel "list of patterns"])
    else do
      let len = length $ SE.extract patternList
      unless (len == patternSize) $ do
        lift $
          raiseError m $
            "The size of the pattern row `"
              <> T.pack (show len)
              <> "` does not match with its input size `"
              <> T.pack (show patternSize)
              <> "`"
      cArrow <- delimiter "=>"
      (body, c) <- rawExpr h
      loc <- getCurrentLoc
      return ((patternList, cArrow, body, loc), c)

rawTermPattern :: Handle -> Parser ((Hint, RP.RawPattern), C)
rawTermPattern h = do
  rawTermPatternBasic h

rawTermPatternBasic :: Handle -> Parser ((Hint, RP.RawPattern), C)
rawTermPatternBasic h =
  choice
    [ rawTermPatternListIntro h,
      rawTermPatternRuneIntro,
      rawTermPatternConsOrVar h
    ]

rawTermPatternListIntro :: Handle -> Parser ((Hint, RP.RawPattern), C)
rawTermPatternListIntro h = do
  m <- getCurrentHint
  (patList, c) <- seriesBracket $ rawTermPattern h
  return ((m, RP.ListIntro patList), c)

rawTermPatternRuneIntro :: Parser ((Hint, RP.RawPattern), C)
rawTermPatternRuneIntro = do
  m <- getCurrentHint
  (s, c) <- rune
  case RU.make s of
    Right r ->
      return ((m, RP.RuneIntro r), c)
    Left e ->
      lift $ raiseError m e

parseName :: Handle -> Parser ((Hint, Name), C)
parseName h = do
  ((m, varText), c) <- var h
  v <- interpretVarName m varText
  return (v, c)

rawTermPatternConsOrVar :: Handle -> Parser ((Hint, RP.RawPattern), C)
rawTermPatternConsOrVar h = do
  ((m, varOrLocator), c1) <- parseName h
  choice
    [ do
        (patArgs, c) <- seriesParen $ rawTermPattern h
        return ((m, RP.Cons varOrLocator c1 (RP.Paren patArgs)), c),
      do
        (kvs, c) <- keyValueArgs $ rawTermPatternKeyValuePair h
        return ((m, RP.Cons varOrLocator c1 (RP.Of kvs)), c),
      do
        return ((m, RP.Var varOrLocator), c1)
    ]

rawTermPatternKeyValuePair :: Handle -> Parser ((Key, (Hint, C, RP.RawPattern)), C)
rawTermPatternKeyValuePair h = do
  mFrom <- getCurrentHint
  (from, c1) <- symbol
  choice
    [ do
        c2 <- delimiter "="
        ((mTo, to), c) <- rawTermPattern h
        return ((from, (mTo, c1 ++ c2, to)), c),
      do
        return ((from, (mFrom, [], RP.Var (Var from))), []) -- record rhyming
    ]

rawTermIf :: Handle -> Parser (RT.RawTerm, C)
rawTermIf h = do
  m <- getCurrentHint
  (ifClause, c1) <- rawTermKeywordClause h "if"
  (elseIfClauseList, c2) <- fmap joinComment $ many $ rawTermKeywordClause h "else-if"
  c3 <- keyword "else"
  (c4, (elseBody, c)) <- betweenBrace $ rawExpr h
  let (elseIfClauseList', elseBody') = adjustComment c1 elseIfClauseList (c2 ++ c3 ++ c4, elseBody)
  return (m :< RT.If ifClause elseIfClauseList' elseBody', c)

adjustComment :: C -> [RT.KeywordClause a] -> RT.EL a -> ([RT.KeywordClause a], RT.EL a)
adjustComment c elseIfClauseList (c', x) = do
  case elseIfClauseList of
    [] ->
      ([], (c ++ c', x))
    clause : rest -> do
      (RT.pushCommentToKeywordClause c clause : rest, (c', x))

joinComment :: [(RT.KeywordClause a, C)] -> ([RT.KeywordClause a], C)
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
          (clause : RT.pushCommentToKeywordClause c clause' : clauseList, trail)

rawTermKeywordClause :: Handle -> T.Text -> Parser (RT.KeywordClause RT.RawTerm, C)
rawTermKeywordClause h k = do
  c1 <- keyword k
  cond <- rawTerm h
  (c2, (body, c3)) <- betweenBrace $ rawExpr h
  return (((c1, cond), (c2, body)), c3)

rawTermWhen :: Handle -> Parser (RT.RawTerm, C)
rawTermWhen h = do
  m <- getCurrentHint
  (whenClause, c) <- rawTermKeywordClause h "when"
  return (m :< RT.When whenClause, c)

rawTermBrace :: Handle -> Parser (RT.RawTerm, C)
rawTermBrace h = do
  m <- getCurrentHint
  (c1, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.Brace c1 e, c)

rawTermWith :: Handle -> Parser (RT.RawTerm, C)
rawTermWith h = do
  m <- getCurrentHint
  (withClause, c) <- rawTermKeywordClause h "with"
  return (m :< RT.With withClause, c)

rawTermBox :: Handle -> Parser (RT.RawTerm, C)
rawTermBox h = do
  m <- getCurrentHint
  c1 <- keyword "meta"
  (t, c) <- rawExpr h
  return (m :< RT.Box t, c1 ++ c)

rawTermBoxIntro :: Handle -> Parser (RT.RawTerm, C)
rawTermBoxIntro h = do
  m <- getCurrentHint
  c1 <- keyword "box"
  vs <- bareSeries Nothing SE.Comma $ rawTermNoeticVar h
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.BoxIntro c1 c2 vs e, c)

rawTermBoxIntroQuote :: Handle -> Parser (RT.RawTerm, C)
rawTermBoxIntroQuote h = do
  m <- getCurrentHint
  c1 <- keyword "quote"
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.BoxIntroQuote c1 c2 e, c)

rawTermBoxNoema :: Handle -> Parser (RT.RawTerm, C)
rawTermBoxNoema h = do
  m <- getCurrentHint
  c1 <- delimiter "&"
  (t, c) <- rawTerm h
  return (m :< RT.BoxNoema t, c1 ++ c)

rawTermFlowIntro :: Handle -> Parser (RT.RawTerm, C)
rawTermFlowIntro h = do
  m <- getCurrentHint
  c1 <- keyword "detach"
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.Detach c1 c2 e, c)

rawTermFlowElim :: Handle -> Parser (RT.RawTerm, C)
rawTermFlowElim h = do
  m <- getCurrentHint
  c1 <- keyword "attach"
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.Attach c1 c2 e, c)

rawTermOption :: Handle -> Parser (RT.RawTerm, C)
rawTermOption h = do
  m <- getCurrentHint
  c1 <- delimiter "?"
  (t, c) <- rawTerm h
  return (m :< RT.Option t, c1 ++ c)

rawTermAdmit :: Parser (RT.RawTerm, C)
rawTermAdmit = do
  m <- getCurrentHint
  c <- keyword "admit"
  return (m :< RT.Admit, c)

rawTermAssert :: Handle -> Parser (RT.RawTerm, C)
rawTermAssert h = do
  m <- getCurrentHint
  c1 <- keyword "assert"
  mText <- getCurrentHint
  (message, c2) <- string
  (c3, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.Assert c1 (mText, message) c2 c3 e, c)

keyValueArgs :: Parser (a, C) -> Parser (SE.Series a, C)
keyValueArgs p = do
  c1 <- keyword "of"
  series (Just ("of", c1)) SE.Brace SE.Comma p

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

preBinder :: Handle -> Parser (RawBinder RT.RawTerm, C)
preBinder h = do
  mxc <- var h
  choice
    [ preAscription h mxc,
      preAscription' h mxc
    ]

preAscription :: Handle -> ((Hint, T.Text), C) -> Parser (RawBinder RT.RawTerm, C)
preAscription h ((m, x), c1) = do
  c2 <- delimiter ":"
  (a, c) <- rawTerm h
  return ((m, x, c1, c2, a), c)

preAscription' :: Handle -> ((Hint, T.Text), C) -> Parser (RawBinder RT.RawTerm, C)
preAscription' h ((m, x), c) = do
  hole <- liftIO $ RT.createHole (gensymHandle h) m
  return ((m, x, c, [], hole), [])

typeWithoutIdent :: Handle -> Parser (RawBinder RT.RawTerm, C)
typeWithoutIdent h = do
  m <- getCurrentHint
  x <- liftIO $ newTextForHole (gensymHandle h)
  (t, c) <- rawTerm h
  return ((m, x, [], [], t), c)

rawTermListIntro :: Handle -> Parser (RT.RawTerm, C)
rawTermListIntro h = do
  m <- getCurrentHint
  (es, c) <- seriesBracket $ rawExpr h
  return (m :< RT.ListIntro es, c)

rawTermPiElimExact :: Handle -> Parser (RT.RawTerm, C)
rawTermPiElimExact h = do
  m <- getCurrentHint
  c1 <- keyword "exact"
  (e, c) <- rawTerm h
  return (m :< RT.PiElimExact c1 e, c)

rawTermIntrospect :: Handle -> Parser (RT.RawTerm, C)
rawTermIntrospect h = do
  m <- getCurrentHint
  c1 <- keyword "introspect"
  (key, c2) <- symbol
  (clauseList, c) <- seriesBraceList $ rawTermIntrospectiveClause h
  return (m :< RT.Introspect c1 key c2 clauseList, c)

rawTermIntrospectiveClause :: Handle -> Parser ((Maybe T.Text, C, RT.RawTerm), C)
rawTermIntrospectiveClause h = do
  (s, cKey) <- symbol
  cArrow <- delimiter "=>"
  (body, c) <- rawExpr h
  if s /= "default"
    then return ((Just s, cKey ++ cArrow, body), c)
    else return ((Nothing, cKey ++ cArrow, body), c)

rawTermIncludeText :: Parser (RT.RawTerm, C)
rawTermIncludeText = do
  m <- getCurrentHint
  c1 <- keyword "include-text"
  (c2, ((mKey, key), c)) <- betweenParen $ do
    mKey <- getCurrentHint
    k <- symbol
    return (mKey, k)
  return (m :< RT.IncludeText c1 c2 mKey key, c)

rawTermSymbol :: Handle -> Parser (RT.RawTerm, C)
rawTermSymbol h = do
  ((m, varOrLocator), c) <- parseVarName h
  return (m :< RT.Var varOrLocator, c)

parseVarName :: Handle -> Parser ((Hint, Name), C)
parseVarName h = do
  ((m, varText), c) <- var h
  v <- interpretVarName m varText
  return (v, c)

interpretVarName :: Hint -> T.Text -> Parser (Hint, Name)
interpretVarName m varText = do
  case DD.getLocatorPair m varText of
    Left _ ->
      return (m, Var varText)
    Right (gl, ll)
      | Just _ :: Maybe Double <- R.readMaybe (T.unpack varText) ->
          return (m, Var varText)
      | otherwise ->
          return (m, Locator (gl, ll))

rawTermTextIntro :: Parser (RT.RawTerm, C)
rawTermTextIntro = do
  m <- getCurrentHint
  (s, c) <- string
  textType <- lift $ locatorToVarGlobal (blur m) coreText
  return (m :< RT.StaticText textType s, c)

rawTermRune :: Parser (RT.RawTerm, C)
rawTermRune = do
  m <- getCurrentHint
  c <- keyword "rune"
  return (m :< RT.Rune, c)

rawTermRuneIntro :: Parser (RT.RawTerm, C)
rawTermRuneIntro = do
  m <- getCurrentHint
  (s, c) <- rune
  runeCons <- lift $ locatorToVarGlobal (blur m) coreRuneRune
  case RU.make s of
    Right r ->
      return (m :< RT.RuneIntro runeCons r, c)
    Left e ->
      lift $ raiseError m e

locatorToVarGlobal :: Hint -> T.Text -> EIO RT.RawTerm
locatorToVarGlobal m text = do
  (gl, ll) <- liftEither $ DD.getLocatorPair (blur m) text
  return $ rawVar (blur m) (Locator (gl, ll))

rawVar :: Hint -> Name -> RT.RawTerm
rawVar m name =
  m :< RT.Var name
