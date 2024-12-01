module Scene.Parse.RawTerm
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

import Context.App
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
import Entity.Name
import Entity.NecessityVariant (NecessityVariant (..), showNecessityVariant)
import Entity.RawBinder
import Entity.RawIdent
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.Rune qualified as RU
import Entity.Syntax.Series qualified as SE
import Scene.Parse.Core
import Text.Megaparsec
import Text.Read qualified as R

rawExpr :: Parser (RT.RawTerm, C)
rawExpr = do
  m <- getCurrentHint
  choice
    [ rawTermLet m,
      rawTermBoxElim m,
      rawTermUse m,
      rawTermPin m,
      do
        e1 <- rawTerm
        choice
          [ do
              c1 <- delimiter ";"
              (e2, c2) <- rawExpr
              return (m :< RT.Seq e1 c1 e2, c2),
            return e1
          ]
    ]

rawTerm :: Parser (RT.RawTerm, C)
rawTerm = do
  choice
    [ rawTermDefine,
      rawTermIntrospect,
      rawTermIncludeText,
      rawTermMagic,
      rawTermMatch,
      rawTermPi,
      rawTermPiIntro,
      rawTermBox,
      rawTermBoxNoema,
      rawTermBoxIntro,
      rawTermBoxIntroQuote,
      rawTermIf,
      rawTermWhen,
      rawTermAssert,
      rawTermOption,
      rawTermFlowIntro,
      rawTermFlowElim,
      rawTermEmbody,
      rawTermWith,
      rawTermPiElimExact,
      do
        m <- getCurrentHint
        ec@(e, c1) <- rawTermSimple
        case e of
          _ :< RT.Var name -> do
            choice
              [ do
                  (kvs, c) <- keyValueArgs rawTermKeyValuePair
                  return (m :< RT.PiElimByKey name c1 kvs, c),
                rawTermPiElimCont ec
              ]
          _ -> do
            rawTermPiElimCont ec
    ]

rawTermProjection :: Parser ([((Hint, T.Text), Loc)], C)
rawTermProjection = do
  choice
    [ do
        c1 <- delimiter "::"
        (mv, c) <- var
        loc <- getCurrentLoc
        (mvs, c') <- rawTermProjection
        return ((mv, loc) : mvs, c1 ++ c ++ c'),
      do
        return ([], [])
    ]

rawTermPiElimCont :: (RT.RawTerm, C) -> Parser (RT.RawTerm, C)
rawTermPiElimCont (e, c) = do
  (projections, cProj) <- rawTermProjection
  let e'@(m :< _) = foldProjection e projections
  argListList <- many $ seriesParen rawExpr
  return $ foldPiElim m (e', c ++ cProj) argListList

foldProjection :: RT.RawTerm -> [((Hint, T.Text), Loc)] -> RT.RawTerm
foldProjection e projections =
  case projections of
    [] ->
      e
    (proj@(mProj, _), loc) : rest ->
      foldProjection (mProj :< RT.Projection e proj loc) rest

{-# INLINE rawTermSimple #-}
rawTermSimple :: Parser (RT.RawTerm, C)
rawTermSimple = do
  choice
    [ rawTermBrace,
      rawTermListIntro,
      rawTermTextIntro,
      rawTermRune,
      rawTermRuneIntro,
      rawTermTau,
      rawTermPointer,
      rawTermVoid,
      rawTermAdmit,
      rawTermHole,
      rawTermSymbol
    ]

rawTermPi :: Parser (RT.RawTerm, C)
rawTermPi = do
  m <- getCurrentHint
  impArgs <- parseImplicitArgs
  expArgs <- seriesParen (choice [try $ var >>= preAscription, typeWithoutIdent])
  cArrow <- delimiter "->"
  (cod, c) <- rawTerm
  loc <- getCurrentLoc
  return (m :< RT.Pi impArgs expArgs cArrow cod loc, c)

rawTermPiIntro :: Parser (RT.RawTerm, C)
rawTermPiIntro = do
  m <- getCurrentHint
  c0 <- keyword "function"
  (defInfo, c) <- parseDef $ do
    return ((), [])
  return (m :< RT.PiIntro c0 defInfo, c)

rawTermKeyValuePair :: Parser ((Hint, Key, C, C, RT.RawTerm), C)
rawTermKeyValuePair = do
  ((m, key), c1) <- var
  choice
    [ do
        c2 <- delimiter "="
        (value, c) <- rawTerm
        return ((m, key, c1, c2, value), c),
      do
        return ((m, key, c1, [], m :< RT.Var (Var key)), [])
    ]

rawTermLet :: Hint -> Parser (RT.RawTerm, C)
rawTermLet mLet = do
  (letKind, c1) <-
    choice
      [ keyword "let" >>= \c1 -> return (RT.Plain False, c1),
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
  loc <- getCurrentLoc
  c7 <- delimiter "in"
  (e2, c) <- rawExpr
  endLoc <- getCurrentLoc
  case (letKind, SE.isEmpty noeticVarList) of
    (RT.Plain _, False) -> do
      return (mLet :< RT.LetOn c1 (mx, patInner, c2, c3, t) c4 noeticVarList c5 e1 c6 loc c7 e2 endLoc, c)
    (_, False) ->
      lift $ Throw.raiseError mLet $ "`on` cannot be used with: `" <> RT.decodeLetKind letKind <> "`"
    _ ->
      return (mLet :< RT.Let letKind c1 (mx, patInner, c2, c3, t) c4 c5 e1 c6 loc c7 e2 endLoc, c)

rawTermBoxElim :: Hint -> Parser (RT.RawTerm, C)
rawTermBoxElim mLet = do
  let keywordReader nv = keyword (showNecessityVariant nv) >>= \c1 -> return (nv, c1)
  (nv, c1) <-
    choice
      [ keywordReader VariantK,
        keywordReader VariantT
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
  loc <- getCurrentLoc
  c7 <- delimiter "in"
  (e2, c) <- rawExpr
  endLoc <- getCurrentLoc
  return (mLet :< RT.BoxElim nv False c1 (mx, patInner, c2, c3, t) c4 noeticVarList c5 e1 c6 loc c7 e2 endLoc, c)

rawTermPin :: Hint -> Parser (RT.RawTerm, C)
rawTermPin m = do
  c1 <- keyword "pin"
  ((mx, x), c2) <- rawTermNoeticVar
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
  (e1, c6) <- rawExpr
  loc <- getCurrentLoc
  c7 <- delimiter "in"
  (e2, c) <- rawExpr
  endLoc <- getCurrentLoc
  return (m :< RT.Pin c1 (mx, x, c2, c3, t) c4 noeticVarList c5 e1 c6 loc c7 e2 endLoc, c)

rawTermUse :: Hint -> Parser (RT.RawTerm, C)
rawTermUse m = do
  c1 <- keyword "use"
  (e, c2) <- rawTerm
  xs@(ys, _) <- seriesBrace preBinder
  c3 <- delimiter "in"
  lift $ ensureIdentLinearity S.empty $ map (\(_, (mx, x, _, _, _)) -> (mx, x)) $ SE.elems ys
  (cont, c) <- rawExpr
  loc <- getCurrentLoc
  return (m :< RT.Use c1 e c2 xs c3 cont loc, c)

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
    [ do
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
          Throw.raiseError m $ "Found a non-linear occurrence of `" <> name <> "`."
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
  (e, c) <- rawTerm
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

rawTermHole :: Parser (RT.RawTerm, C)
rawTermHole = do
  m <- getCurrentHint
  c <- keyword "_"
  h <- lift $ Gensym.newPreHole m
  return (h, c)

parseDef :: Parser (a, C) -> Parser (RT.RawDef a, C)
parseDef nameParser = do
  (geist, c1) <- parseGeist nameParser
  (c2, ((e, c3), loc, c)) <- betweenBrace' rawExpr
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

parseGeist :: Parser (a, C) -> Parser (RT.RawGeist a, C)
parseGeist nameParser = do
  loc <- getCurrentHint
  (name', c1) <- nameParser
  impArgs <- parseImplicitArgs
  (isConstLike, expArgs@(expSeries, _)) <- do
    choice
      [ do
          expDomArgList <- seriesParen preBinder
          return (False, expDomArgList),
        return (True, (SE.emptySeries (Just SE.Paren) SE.Comma, []))
      ]
  lift $ ensureArgumentLinearity S.empty $ map (\(mx, x, _, _, _) -> (mx, x)) $ SE.extract expSeries
  m <- getCurrentHint
  (c2, (cod, c)) <- parseDefInfoCod m
  return (RT.RawGeist {loc, name = (name', c1), isConstLike, impArgs, expArgs, cod = (c2, cod)}, c)

parseImplicitArgs :: Parser (SE.Series (RawBinder RT.RawTerm), C)
parseImplicitArgs =
  choice
    [ parseImplicitArgs',
      return (SE.emptySeries (Just SE.Angle) SE.Comma, [])
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
          Throw.raiseError m $ "Found a non-linear occurrence of `" <> name <> "`."
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
  (defInfo, c) <- parseDef $ do
    (name, c1) <- baseName
    name' <- lift $ adjustHoleVar name
    return (name', c1)
  return (m :< RT.PiIntroFix c0 defInfo, c)

adjustHoleVar :: BN.BaseName -> App T.Text
adjustHoleVar bn = do
  let bn' = BN.reify bn
  if bn' /= "_"
    then return bn'
    else Gensym.newTextForHole

rawTermMagic :: Parser (RT.RawTerm, C)
rawTermMagic = do
  m <- getCurrentHint
  c <- keyword "magic"
  choice
    [ rawTermMagicCast m c,
      rawTermMagicStore m c,
      rawTermMagicLoad m c,
      rawTermMagicAlloca m c,
      rawTermMagicExternal m c,
      rawTermMagicOpaqueValue m c,
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
    c6 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Cast c1 (c2, castFrom) (c3, castTo) (c4, value) c6)

rawTermMagicStore :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicStore m c = do
  rawTermMagicBase "store" $ do
    t <- rawExpr
    c3 <- delimiter ","
    value <- rawExpr
    c4 <- delimiter ","
    pointer <- rawExpr
    c5 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Store c1 (c2, t) (c3, value) (c4, pointer) c5)

rawTermMagicLoad :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicLoad m c = do
  rawTermMagicBase "load" $ do
    t <- rawExpr
    c3 <- delimiter ","
    pointer <- rawExpr
    c4 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Load c1 (c2, t) (c3, pointer) c4)

rawTermMagicAlloca :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicAlloca m c = do
  rawTermMagicBase "alloca" $ do
    t <- rawExpr
    c3 <- delimiter ","
    size <- rawExpr
    c4 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Alloca c1 (c2, t) (c3, size) c4)

rawTermMagicExternal :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicExternal m c0 = do
  c1 <- keyword "external"
  mUse <- getCurrentHint
  (extFunName, cExt) <- symbol
  let extFunName' = EN.ExternalName extFunName
  (es, c2) <- seriesParen rawExpr
  choice
    [ do
        (s, c) <- seriesParen rawExprAndLowType
        return (m :< RT.Magic c0 (RT.External c1 mUse extFunName' cExt es (Just (c2, s))), c),
      return (m :< RT.Magic c0 (RT.External c1 mUse extFunName' cExt es Nothing), c2)
    ]

rawExprAndLowType :: Parser (RT.VarArg, C)
rawExprAndLowType = do
  m <- getCurrentHint
  (e, c1) <- rawExpr
  c2 <- delimiter ":"
  (t, c) <- rawTerm
  return ((m, e, c1, c2, t), c)

rawTermMagicGlobal :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGlobal m c = do
  rawTermMagicBase "global" $ do
    (globalVarName, c3) <- string
    c4 <- delimiter ","
    lt <- rawExpr
    c5 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Global c1 (c2, (EN.ExternalName globalVarName, c3)) (c4, lt) c5)

rawTermMagicOpaqueValue :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicOpaqueValue m c0 = do
  c1 <- keyword "opaque-value"
  (c2, (e, c)) <- betweenBrace rawExpr
  return (m :< RT.Magic c0 (RT.OpaqueValue c1 (c2, e)), c)

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
  es <- bareSeries Nothing SE.Comma rawTerm
  (patternRowList, c) <- seriesBraceList $ rawTermPatternRow (length $ SE.extract es)
  return (m :< RT.DataElim c1 isNoetic es patternRowList, c)

rawTermPatternRow :: Int -> Parser (RP.RawPatternRow RT.RawTerm, C)
rawTermPatternRow patternSize = do
  m <- getCurrentHint
  patternList <- bareSeries Nothing SE.Comma rawTermPattern
  if SE.isEmpty patternList
    then failure Nothing (S.fromList [asLabel "list of patterns"])
    else do
      let len = length $ SE.extract patternList
      unless (len == patternSize) $ do
        lift $
          Throw.raiseError m $
            "The size of the pattern row `"
              <> T.pack (show len)
              <> "` does not match with its input size `"
              <> T.pack (show patternSize)
              <> "`"
      cArrow <- delimiter "=>"
      (body, c) <- rawExpr
      loc <- getCurrentLoc
      return ((patternList, cArrow, body, loc), c)

rawTermPattern :: Parser ((Hint, RP.RawPattern), C)
rawTermPattern = do
  rawTermPatternBasic

rawTermPatternBasic :: Parser ((Hint, RP.RawPattern), C)
rawTermPatternBasic =
  choice
    [ rawTermPatternListIntro,
      rawTermPatternRuneIntro,
      rawTermPatternConsOrVar
    ]

rawTermPatternListIntro :: Parser ((Hint, RP.RawPattern), C)
rawTermPatternListIntro = do
  m <- getCurrentHint
  (patList, c) <- seriesBracket rawTermPattern
  return ((m, RP.ListIntro patList), c)

rawTermPatternRuneIntro :: Parser ((Hint, RP.RawPattern), C)
rawTermPatternRuneIntro = do
  m <- getCurrentHint
  (s, c) <- rune
  case RU.make s of
    Right r ->
      return ((m, RP.RuneIntro r), c)
    Left e ->
      lift $ Throw.raiseError m e

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
  (ifClause, c1) <- rawTermKeywordClause "if"
  (elseIfClauseList, c2) <- fmap joinComment $ many $ rawTermKeywordClause "else-if"
  c3 <- keyword "else"
  (c4, (elseBody, c)) <- betweenBrace rawExpr
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

rawTermKeywordClause :: T.Text -> Parser (RT.KeywordClause RT.RawTerm, C)
rawTermKeywordClause k = do
  c1 <- keyword k
  cond <- rawTerm
  (c2, (body, c3)) <- betweenBrace rawExpr
  return (((c1, cond), (c2, body)), c3)

rawTermWhen :: Parser (RT.RawTerm, C)
rawTermWhen = do
  m <- getCurrentHint
  (whenClause, c) <- rawTermKeywordClause "when"
  return (m :< RT.When whenClause, c)

rawTermBrace :: Parser (RT.RawTerm, C)
rawTermBrace = do
  m <- getCurrentHint
  (c1, (e, c)) <- betweenBrace rawExpr
  return (m :< RT.Brace c1 e, c)

rawTermWith :: Parser (RT.RawTerm, C)
rawTermWith = do
  m <- getCurrentHint
  (withClause, c) <- rawTermKeywordClause "with"
  return (m :< RT.With withClause, c)

rawTermBox :: Parser (RT.RawTerm, C)
rawTermBox = do
  m <- getCurrentHint
  c1 <- keyword "meta"
  (t, c) <- rawExpr
  return (m :< RT.Box t, c1 ++ c)

rawTermBoxIntro :: Parser (RT.RawTerm, C)
rawTermBoxIntro = do
  m <- getCurrentHint
  c1 <- keyword "box"
  vs <- bareSeries Nothing SE.Comma rawTermNoeticVar
  (c2, (e, c)) <- betweenBrace rawExpr
  return (m :< RT.BoxIntro c1 c2 vs e, c)

rawTermBoxIntroQuote :: Parser (RT.RawTerm, C)
rawTermBoxIntroQuote = do
  m <- getCurrentHint
  c1 <- keyword "quote"
  (c2, (e, c)) <- betweenBrace rawExpr
  return (m :< RT.BoxIntroQuote c1 c2 e, c)

rawTermBoxNoema :: Parser (RT.RawTerm, C)
rawTermBoxNoema = do
  m <- getCurrentHint
  c1 <- delimiter "&"
  (t, c) <- rawTerm
  return (m :< RT.BoxNoema t, c1 ++ c)

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
  (t, c) <- rawTerm
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

preBinder :: Parser (RawBinder RT.RawTerm, C)
preBinder = do
  mxc <- var
  choice
    [ preAscription mxc,
      preAscription' mxc
    ]

preAscription :: ((Hint, T.Text), C) -> Parser (RawBinder RT.RawTerm, C)
preAscription ((m, x), c1) = do
  c2 <- delimiter ":"
  (a, c) <- rawTerm
  return ((m, x, c1, c2, a), c)

preAscription' :: ((Hint, T.Text), C) -> Parser (RawBinder RT.RawTerm, C)
preAscription' ((m, x), c) = do
  h <- lift $ Gensym.newPreHole m
  return ((m, x, c, [], h), [])

typeWithoutIdent :: Parser (RawBinder RT.RawTerm, C)
typeWithoutIdent = do
  m <- getCurrentHint
  x <- lift Gensym.newTextForHole
  (t, c) <- rawTerm
  return ((m, x, [], [], t), c)

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

rawTermIncludeText :: Parser (RT.RawTerm, C)
rawTermIncludeText = do
  m <- getCurrentHint
  c1 <- keyword "include-text"
  (c2, ((mKey, key), c)) <- betweenParen $ do
    mKey <- getCurrentHint
    k <- symbol
    return (mKey, k)
  return (m :< RT.IncludeText c1 c2 mKey key, c)

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
      lift $ Throw.raiseError m e

locatorToVarGlobal :: Hint -> T.Text -> App RT.RawTerm
locatorToVarGlobal m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair (blur m) text
  return $ rawVar (blur m) (Locator (gl, ll))

rawVar :: Hint -> Name -> RT.RawTerm
rawVar m name =
  m :< RT.Var name
