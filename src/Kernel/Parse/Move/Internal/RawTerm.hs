module Kernel.Parse.Move.Internal.RawTerm
  ( Handle (..),
    new,
    rawExpr,
    rawTerm,
    var,
    preAscription,
    preBinder,
    parseDef,
    parseGeist,
    parseDefInfoCod,
    typeWithoutIdent,
    parseImplicitArgs,
    keyword,
    baseName,
  )
where

import CodeParser.Move.GetInfo
import CodeParser.Move.Parse
import CodeParser.Rule.Parser
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.Trans
import Data.Set qualified as S
import Data.Text qualified as T
import Error.Move.Run (raiseError)
import Error.Rule.EIO (EIO)
import Gensym.Rule.Handle qualified as Gensym
import Kernel.Common.Rule.Const
import Language.Common.Move.CreateSymbol (newTextForHole)
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.Rune qualified as RU
import Language.RawTerm.Move.CreateHole qualified as RT
import Language.RawTerm.Rule.Key
import Language.RawTerm.Rule.Name
import Language.RawTerm.Rule.NecessityVariant qualified as NV
import Language.RawTerm.Rule.RawBinder
import Language.RawTerm.Rule.RawIdent
import Language.RawTerm.Rule.RawPattern qualified as RP
import Language.RawTerm.Rule.RawTerm qualified as RT
import Logger.Rule.Hint
import SyntaxTree.Move.ParseSeries
import SyntaxTree.Rule.Block
import SyntaxTree.Rule.C
import SyntaxTree.Rule.Series qualified as SE
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Read qualified as R

newtype Handle = Handle
  { gensymHandle :: Gensym.Handle
  }

new :: Gensym.Handle -> Handle
new gensymHandle = do
  Handle {..}

rawExpr :: Handle -> Parser (RT.RawTerm, C)
rawExpr h = do
  m <- getCurrentHint
  (headSymbol, c) <- symbol'
  case RT.letKindFromText headSymbol of
    Just kind ->
      rawTermLet h m kind c
    Nothing -> do
      case NV.fromText headSymbol of
        Just variant ->
          rawTermBoxElim h m variant c
        Nothing ->
          case headSymbol of
            "pin" ->
              rawTermPin h m c
            _ -> do
              e1 <- rawTerm' h m headSymbol c
              choice
                [ do
                    c1 <- delimiter ";"
                    mExpr <- optional $ rawExpr h
                    case mExpr of
                      Just (e2, c2) ->
                        return (m :< RT.Seq e1 c1 e2, c2)
                      Nothing ->
                        return (m :< RT.SeqEnd (fst e1), snd e1 ++ c1),
                  return e1
                ]

rawTerm :: Handle -> Parser (RT.RawTerm, C)
rawTerm h = do
  m <- getCurrentHint
  (headSymbol, c) <- symbol'
  rawTerm' h m headSymbol c

rawTerm' :: Handle -> Hint -> T.Text -> C -> Parser (RT.RawTerm, C)
rawTerm' h m headSymbol c = do
  case headSymbol of
    "define" -> do
      rawTermDefine h m c
    "introspect" -> do
      rawTermIntrospect h m c
    "include-text" -> do
      rawTermIncludeText m c
    "magic" -> do
      rawTermMagic h m c
    "match" -> do
      rawTermMatch h m c False
    "case" -> do
      rawTermMatch h m c True
    "function" -> do
      rawTermPiIntro h m c
    "meta" -> do
      rawTermBox h m c
    "box" -> do
      rawTermBoxIntro h m c
    "quote" -> do
      rawTermBoxIntroQuote h m c
    "assert" -> do
      rawTermAssert h m c
    "detach" -> do
      rawTermFlowIntro h m c
    "attach" -> do
      rawTermFlowElim h m c
    "exact" -> do
      rawTermPiElimExact h m c
    "if" -> do
      rawTermIf h m c
    "when" -> do
      rawTermWhen h m c
    "rune" -> do
      rawTermRune m c
    "type" -> do
      rawTermTau m c
    "pointer" -> do
      rawTermPointer m c
    "void" -> do
      rawTermVoid m c
    "admit" -> do
      rawTermAdmit m c
    "_" -> do
      rawTermHole h m c
    _ -> do
      if T.null headSymbol
        then do
          choice
            [ rawTermPi h,
              rawTermBrace h,
              rawTermListIntro h,
              rawTermTextIntro,
              rawTermRuneIntro,
              rawTermBoxNoema h,
              rawTermOption h,
              rawTermEmbody h
            ]
        else do
          name <- interpretVarName m headSymbol
          choice
            [ do
                (kvs, c') <- keyValueArgs $ rawTermKeyValuePair h
                return (m :< RT.PiElimByKey name c kvs, c'),
              rawTermPiElimCont h (m :< RT.Var name, c)
            ]

rawTermPiElimCont :: Handle -> (RT.RawTerm, C) -> Parser (RT.RawTerm, C)
rawTermPiElimCont h (e@(m :< _), c) = do
  argListList <- many $ seriesParen (rawTerm h)
  return $ foldPiElim m (e, c) argListList

rawTermPi :: Handle -> Parser (RT.RawTerm, C)
rawTermPi h = do
  m <- getCurrentHint
  impArgs <- parseImplicitArgs h
  expArgs <- seriesParen (choice [try $ var h >>= preAscription h, typeWithoutIdent h])
  cArrow <- delimiter "->"
  (cod, c) <- rawTerm h
  loc <- getCurrentLoc
  return (m :< RT.Pi impArgs expArgs cArrow cod loc, c)

rawTermPiIntro :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermPiIntro h m c0 = do
  (defInfo, c) <- parseDef h $ do
    mName <- optional symbol
    case mName of
      Nothing ->
        return (Nothing, [])
      Just (name, c) ->
        return (Just name, c)
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

rawTermLet :: Handle -> Hint -> RT.LetKind -> C -> Parser (RT.RawTerm, C)
rawTermLet h mLet letKind c1 = do
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

rawTermBoxElim :: Handle -> Hint -> NV.NecessityVariant -> C -> Parser (RT.RawTerm, C)
rawTermBoxElim h mLet nv c1 = do
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

rawTermPin :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermPin h m c1 = do
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

rawTermTau :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermTau m c = do
  return (m :< RT.Tau, c)

rawTermPointer :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermPointer m c = do
  return (m :< RT.Pointer, c)

rawTermVoid :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermVoid m c = do
  return (m :< RT.Void, c)

rawTermHole :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermHole h m c = do
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

rawTermDefine :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermDefine h m c0 = do
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

rawTermMagic :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagic h m c = do
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
    castFrom <- rawTerm h
    c3 <- delimiter ","
    castTo <- rawTerm h
    c4 <- delimiter ","
    value <- rawTerm h
    c6 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Cast c1 (c2, castFrom) (c3, castTo) (c4, value) c6)

rawTermMagicStore :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicStore h m c = do
  rawTermMagicBase "store" $ do
    t <- rawTerm h
    c3 <- delimiter ","
    value <- rawTerm h
    c4 <- delimiter ","
    pointer <- rawTerm h
    c5 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Store c1 (c2, t) (c3, value) (c4, pointer) c5)

rawTermMagicLoad :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicLoad h m c = do
  rawTermMagicBase "load" $ do
    t <- rawTerm h
    c3 <- delimiter ","
    pointer <- rawTerm h
    c4 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Load c1 (c2, t) (c3, pointer) c4)

rawTermMagicAlloca :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicAlloca h m c = do
  rawTermMagicBase "alloca" $ do
    t <- rawTerm h
    c3 <- delimiter ","
    size <- rawTerm h
    c4 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Alloca c1 (c2, t) (c3, size) c4)

rawTermMagicExternal :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicExternal h m c0 = do
  c1 <- keyword "external"
  mUse <- getCurrentHint
  (extFunName, cExt) <- symbol
  let extFunName' = EN.ExternalName extFunName
  (es, c2) <- seriesParen (rawTerm h)
  choice
    [ do
        (s, c) <- seriesParen (rawTermAndLowType h)
        return (m :< RT.Magic c0 (RT.External c1 mUse extFunName' cExt es (Just (c2, s))), c),
      return (m :< RT.Magic c0 (RT.External c1 mUse extFunName' cExt es Nothing), c2)
    ]

rawTermAndLowType :: Handle -> Parser (RT.VarArg, C)
rawTermAndLowType h = do
  m <- getCurrentHint
  (e, c1) <- rawTerm h
  c2 <- delimiter ":"
  (t, c) <- rawTerm h
  return ((m, e, c1, c2, t), c)

rawTermMagicGlobal :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGlobal h m c = do
  rawTermMagicBase "global" $ do
    (globalVarName, c3) <- string
    c4 <- delimiter ","
    lt <- rawTerm h
    c5 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Global c1 (c2, (EN.ExternalName globalVarName, c3)) (c4, lt) c5)

rawTermMagicOpaqueValue :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicOpaqueValue h m c0 = do
  c1 <- keyword "opaque-value"
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.Magic c0 (RT.OpaqueValue c1 (c2, e)), c)

rawTermMatch :: Handle -> Hint -> C -> Bool -> Parser (RT.RawTerm, C)
rawTermMatch h m c1 isNoetic = do
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
      return ((patternList, cArrow, body), c)

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
  return ((m, v), c)

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

rawTermIf :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermIf h m c0 = do
  (ifClause, c1) <- rawTermClause h c0
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

rawTermClause :: Handle -> C -> Parser (RT.KeywordClause RT.RawTerm, C)
rawTermClause h c1 = do
  cond <- rawTerm h
  (c2, (body, c3)) <- betweenBrace $ rawExpr h
  return (((c1, cond), (c2, body)), c3)

rawTermWhen :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermWhen h m c0 = do
  (whenClause, c) <- rawTermClause h c0
  return (m :< RT.When whenClause, c)

rawTermBrace :: Handle -> Parser (RT.RawTerm, C)
rawTermBrace h = do
  m <- getCurrentHint
  (c1, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.Brace c1 e, c)

rawTermBox :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermBox h m c1 = do
  (t, c) <- rawTerm h
  return (m :< RT.Box t, c1 ++ c)

rawTermBoxIntro :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermBoxIntro h m c1 = do
  vs <- bareSeries Nothing SE.Comma $ rawTermNoeticVar h
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.BoxIntro c1 c2 vs e, c)

rawTermBoxIntroQuote :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermBoxIntroQuote h m c1 = do
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.BoxIntroQuote c1 c2 e, c)

rawTermBoxNoema :: Handle -> Parser (RT.RawTerm, C)
rawTermBoxNoema h = do
  m <- getCurrentHint
  c1 <- delimiter "&"
  (t, c) <- rawTerm h
  return (m :< RT.BoxNoema t, c1 ++ c)

rawTermFlowIntro :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermFlowIntro h m c1 = do
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.Detach c1 c2 e, c)

rawTermFlowElim :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermFlowElim h m c1 = do
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.Attach c1 c2 e, c)

rawTermOption :: Handle -> Parser (RT.RawTerm, C)
rawTermOption h = do
  m <- getCurrentHint
  c1 <- delimiter "?"
  (t, c) <- rawTerm h
  return (m :< RT.Option t, c1 ++ c)

rawTermAdmit :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermAdmit m c = do
  return (m :< RT.Admit, c)

rawTermAssert :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermAssert h m c1 = do
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
  (es, c) <- seriesBracket $ rawTerm h
  return (m :< RT.ListIntro es, c)

rawTermPiElimExact :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermPiElimExact h m c1 = do
  (e, c) <- rawTerm h
  return (m :< RT.PiElimExact c1 e, c)

rawTermIntrospect :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermIntrospect h m c1 = do
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

rawTermIncludeText :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermIncludeText m c1 = do
  (c2, ((mKey, key), c)) <- betweenParen $ do
    mKey <- getCurrentHint
    k <- symbol
    return (mKey, k)
  return (m :< RT.IncludeText c1 c2 mKey key, c)

interpretVarName :: Hint -> T.Text -> Parser Name
interpretVarName m varText = do
  case DD.getLocatorPair m varText of
    Left _ ->
      return (Var varText)
    Right (gl, ll)
      | Just _ :: Maybe Double <- R.readMaybe (T.unpack varText) ->
          return (Var varText)
      | otherwise ->
          return (Locator (gl, ll))

rawTermTextIntro :: Parser (RT.RawTerm, C)
rawTermTextIntro = do
  m <- getCurrentHint
  (s, c) <- string
  textType <- lift $ locatorToVarGlobal (blur m) coreText
  return (m :< RT.StaticText textType s, c)

rawTermRune :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermRune m c = do
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

var :: Handle -> Parser ((Hint, T.Text), C)
var h = do
  m <- getCurrentHint
  (x, c) <- symbol
  if x /= "_"
    then return ((m, x), c)
    else do
      unusedVar <- liftIO $ newTextForHole (gensymHandle h)
      return ((m, unusedVar), c)

baseName :: Parser (BN.BaseName, C)
baseName = do
  lexeme $ do
    bn <- takeWhile1P Nothing (`S.notMember` nonBaseNameCharSet)
    return $ BN.fromText bn

keyword :: T.Text -> Parser C
keyword expected = do
  fmap snd $ lexeme $ try $ do
    _ <- chunk expected
    label (T.unpack expected) $ notFollowedBy symbol

rune :: Parser (T.Text, C)
rune =
  lexeme $ do
    _ <- char '`'
    runeInner []

runeInner :: [Char] -> Parser T.Text
runeInner acc = do
  c <- anySingle
  case c of
    '\\' -> do
      c' <- anySingle
      if c' == '`'
        then runeInner (c' : acc)
        else runeInner (c' : '\\' : acc)
    '`' ->
      return $ T.pack $ Prelude.reverse acc
    _ ->
      runeInner (c : acc)

betweenParen :: Parser a -> Parser (C, (a, C))
betweenParen p = do
  c1 <- delimiter "("
  v <- p
  c2 <- delimiter ")"
  return (c1, (v, c2))

betweenBrace :: Parser a -> Parser (C, (a, C))
betweenBrace p = do
  c1 <- delimiter "{"
  v <- p
  c2 <- delimiter "}"
  return (c1, (v, c2))

betweenBrace' :: Parser a -> Parser (Block' a)
betweenBrace' p = do
  c1 <- delimiter "{"
  v <- p
  loc <- getCurrentLoc
  c2 <- delimiter "}"
  return (c1, (v, loc, c2))

{-# INLINE nonBaseNameCharSet #-}
nonBaseNameCharSet :: S.Set Char
nonBaseNameCharSet =
  S.insert nsSepChar nonSymbolCharSet
