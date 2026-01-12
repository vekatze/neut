module Kernel.Parse.Internal.RawTerm
  ( Handle (..),
    new,
    rawExpr,
    rawTerm,
    rawType,
    var,
    preAscription,
    preBinder,
    parseDef,
    parseAliasDef,
    parseGeist,
    parseAliasGeist,
    parseDefInfoCod,
    typeWithoutIdent,
    parseImplicitParams,
    parseDefaultParams,
    keyword,
    baseName,
  )
where

import App.App (App)
import App.Run (raiseError)
import CodeParser.GetInfo
import CodeParser.Parser
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.Trans
import Data.Set qualified as S
import Data.Text qualified as T
import Gensym.Handle qualified as Gensym
import Kernel.Common.Const
import Language.Common.BaseName qualified as BN
import Language.Common.CreateSymbol (newTextForHole)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.Opacity qualified as O
import Language.Common.Rune qualified as RU
import Language.RawTerm.CreateHole qualified as RT
import Language.RawTerm.Key
import Language.RawTerm.Name
import Language.RawTerm.NecessityVariant qualified as NV
import Language.RawTerm.RawBinder
import Language.RawTerm.RawIdent
import Language.RawTerm.RawPattern qualified as RP
import Language.RawTerm.RawTerm qualified as RT
import Logger.Hint
import SyntaxTree.Block
import SyntaxTree.C
import SyntaxTree.ParseSeries
import SyntaxTree.Series qualified as SE
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
            "letveil" ->
              rawTermTauElim h m c
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

rawType :: Handle -> Parser (RT.RawType, C)
rawType h = do
  m <- getCurrentHint
  (headSymbol, c) <- symbol'
  rawType' h m headSymbol c

rawType' :: Handle -> Hint -> T.Text -> C -> Parser (RT.RawType, C)
rawType' h m headSymbol c =
  case headSymbol of
    "type" ->
      rawTypeTau m c
    "rune" ->
      rawTypeRune m c
    "pointer" ->
      rawTypePointer m c
    "void" ->
      rawTypeVoid m c
    "introspect" ->
      rawTypeIntrospect h m c
    "_" ->
      rawTypeHole h m c
    _ ->
      if T.null headSymbol
        then do
          choice
            [ rawTypePi h,
              rawTypeBox h,
              rawTypeBoxNoema h,
              rawTypeCode h,
              rawTypeOption h
            ]
        else do
          name <- interpretTypeName m headSymbol
          rawTypeTyAppCont h (m :< RT.TyVar name, c)

rawTerm' :: Handle -> Hint -> T.Text -> C -> Parser (RT.RawTerm, C)
rawTerm' h m headSymbol c = do
  case headSymbol of
    "define" -> do
      rawTermDefine h O.Opaque m c
    "macro" -> do
      rawTermDefine h O.Clear m c
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
    "box" -> do
      rawTermBoxIntro h m c
    "lift" -> do
      rawTermBoxIntroLift h m c
    "quote" -> do
      rawTermCodeIntro h RT.CodeVariantK m c
    "promote" ->
      rawTermCodeIntro h RT.CodeVariantC m c
    "unquote" -> do
      rawTermCodeElim h m c
    "veil" -> do
      rawTermTauIntro h m c
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
    "admit" -> do
      rawTermAdmit m c
    _ -> do
      if T.null headSymbol
        then do
          choice
            [ rawTermBrace h,
              rawTermTextIntro,
              rawTermRuneIntro,
              rawTermEmbody h
            ]
        else do
          name <- interpretVarName m headSymbol
          choice
            [ do
                (kvs, c') <- keyValueArgs $ rawTermKeyValuePair h
                return (m :< RT.PiElimByKey name c kvs, c'),
              do
                (es, c') <- metaPiElim $ rawTerm h
                return (m :< RT.PiElimMeta name c es, c'),
              do
                (es, c') <- seriesBracket $ rawTerm h
                return (m :< RT.PiElimRule name c es, c'),
              do
                rawTermPiElimCont h (m :< RT.Var name, c)
            ]

rawTermPiElimCont :: Handle -> (RT.RawTerm, C) -> Parser (RT.RawTerm, C)
rawTermPiElimCont h (e@(m :< _), c) = do
  argListList <- many $ rawTermPiElimArgs h
  return $ foldPiElim m (e, c) argListList

type PiElimDefaultArgs =
  SE.Series (Hint, Key, C, C, RT.RawTerm)

type PiElimArgs =
  ( Maybe (SE.Series RT.RawType),
    C,
    SE.Series RT.RawTerm,
    C,
    Maybe PiElimDefaultArgs
  )

type PiElimArgList =
  (PiElimArgs, C)

rawTermPiElimArgs ::
  Handle ->
  Parser PiElimArgList
rawTermPiElimArgs h = do
  (mImpArgs, c1) <- parseImplicitArgsMaybe h
  (expArgs, c2) <- seriesParen (rawTerm h)
  mDefaultArgs <- optional $ seriesBracket $ rawTermKeyValuePair h
  case mDefaultArgs of
    Nothing ->
      return ((mImpArgs, c1, expArgs, c2, Nothing), [])
    Just (defaultArgs, c3) ->
      return ((mImpArgs, c1, expArgs, c2, Just defaultArgs), c3)

rawTypeTyAppCont :: Handle -> (RT.RawType, C) -> Parser (RT.RawType, C)
rawTypeTyAppCont h (t@(m :< _), c) = do
  argListList <- many $ seriesParen (rawType h)
  return $ foldTyApp m (t, c) argListList

rawTypePi :: Handle -> Parser (RT.RawType, C)
rawTypePi h = do
  m <- getCurrentHint
  impArgs <- parseImplicitParams h
  expArgs <- seriesParen (choice [try $ var h >>= preAscription h, typeWithoutIdent h])
  defaultArgs <- parseDefaultParams h
  cArrow <- delimiter "->"
  (cod, c) <- rawType h
  loc <- getCurrentLoc
  return (m :< RT.Pi impArgs expArgs defaultArgs cArrow cod loc, c)

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
        c2 <- delimiter ":="
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
          vs <- bareSeries SE.Comma $ rawTermNoeticVar h
          return $ SE.pushComment c vs,
        return $ SE.emptySeries' Nothing SE.Comma
      ]
  c5 <- delimiter "="
  lift $ ensureIdentLinearity S.empty $ SE.extract noeticVarList
  (e1, c6) <- rawTerm h
  loc <- getCurrentLoc
  c7 <- delimiter ";"
  (e2, c) <- rawExpr h
  endLoc <- getCurrentLoc
  if SE.isEmpty noeticVarList
    then return (mLet :< RT.Let letKind c1 (mx, patInner, c2, c3, t) c4 c5 e1 c6 loc c7 e2 endLoc, c)
    else return (mLet :< RT.LetOn letKind c1 (mx, patInner, c2, c3, t) c4 noeticVarList c5 e1 c6 loc c7 e2 endLoc, c)

rawTermBoxElim :: Handle -> Hint -> NV.NecessityVariant -> C -> Parser (RT.RawTerm, C)
rawTermBoxElim h mLet nv c1 = do
  ((mx, patInner), c2) <- rawTermPattern h
  (c3, (t, c4)) <- rawTermLetVarAscription h mx
  noeticVarList <-
    choice
      [ do
          c <- keyword "on"
          vs <- bareSeries SE.Comma $ rawTermNoeticVar h
          return $ SE.pushComment c vs,
        return $ SE.emptySeries' Nothing SE.Comma
      ]
  c5 <- delimiter "="
  lift $ ensureIdentLinearity S.empty $ SE.extract noeticVarList
  (e1, c6) <- rawTerm h
  loc <- getCurrentLoc
  c7 <- delimiter ";"
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
          vs <- bareSeries SE.Comma $ rawTermNoeticVar h
          return $ SE.pushComment c vs,
        return $ SE.emptySeries' Nothing SE.Comma
      ]
  c5 <- delimiter "="
  (e1, c6) <- rawTerm h
  loc <- getCurrentLoc
  c7 <- delimiter ";"
  (e2, c) <- rawExpr h
  endLoc <- getCurrentLoc
  return (m :< RT.Pin c1 (mx, x, c2, c3, t) c4 noeticVarList c5 e1 c6 loc c7 e2 endLoc, c)

rawTermTauElim :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermTauElim h m c1 = do
  ((mx, x), c2) <- var h
  c3 <- delimiter "="
  (e1, c4) <- rawTerm h
  loc <- getCurrentLoc
  c5 <- delimiter ";"
  (e2, c) <- rawExpr h
  endLoc <- getCurrentLoc
  return (m :< RT.TauElim c1 (mx, x, c2) c3 e1 c4 loc c5 e2 endLoc, c)

rawTermLetVarAscription :: Handle -> Hint -> Parser (C, (RT.RawType, C))
rawTermLetVarAscription h m = do
  (c, mtc) <- rawTermLetVarAscription' h
  case mtc of
    Just tc ->
      return (c, tc)
    Nothing -> do
      t <- liftIO $ RT.createTypeHole (gensymHandle h) m
      return (c, (t, []))

rawTermLetVarAscription' :: Handle -> Parser (C, Maybe (RT.RawType, C))
rawTermLetVarAscription' h =
  choice
    [ do
        c <- delimiter ":"
        tc <- rawType h
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

rawTypeTau :: Hint -> C -> Parser (RT.RawType, C)
rawTypeTau m c = do
  return (m :< RT.Tau, c)

rawTypePointer :: Hint -> C -> Parser (RT.RawType, C)
rawTypePointer m c = do
  return (m :< RT.Pointer, c)

rawTypeVoid :: Hint -> C -> Parser (RT.RawType, C)
rawTypeVoid m c = do
  return (m :< RT.Void, c)

rawTypeHole :: Handle -> Hint -> C -> Parser (RT.RawType, C)
rawTypeHole h m c = do
  hole <- liftIO $ RT.createTypeHole (gensymHandle h) m
  return (hole, c)

rawTypeIntrospect :: Handle -> Hint -> C -> Parser (RT.RawType, C)
rawTypeIntrospect h m c1 = do
  (key, c2) <- symbol
  (clauseList, c) <- seriesBraceList $ rawTypeIntrospectiveClause h
  return (m :< RT.TyIntrospect c1 key c2 clauseList, c)

rawTypeIntrospectiveClause :: Handle -> Parser ((Maybe T.Text, C, RT.RawType), C)
rawTypeIntrospectiveClause h = do
  (s, cKey) <- symbol
  cArrow <- delimiter "=>"
  (body, c) <- rawType h
  if s /= "default"
    then return ((Just s, cKey ++ cArrow, body), c)
    else return ((Nothing, cKey ++ cArrow, body), c)

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

parseAliasDef :: Handle -> Parser (a, C) -> Parser (RT.RawTypeDef a, C)
parseAliasDef h nameParser = do
  (geist, c1) <- parseAliasGeist h nameParser
  (c2, ((t, c3), loc, c)) <- betweenBrace' $ rawType h
  return
    ( RT.RawTypeDef
        { typeGeist = geist,
          typeLeadingComment = c1 ++ c2,
          typeBody = t,
          typeTrailingComment = c3,
          typeEndLoc = loc
        },
      c
    )

parseGeist :: Handle -> Parser (a, C) -> Parser (RT.RawGeist a, C)
parseGeist h nameParser = do
  loc <- getCurrentHint
  (name', c1) <- nameParser
  impArgs <- parseImplicitParams h
  (isConstLike, expArgs@(expSeries, _), defaultArgs) <- do
    choice
      [ do
          expDomArgList <- seriesParen $ preBinder h
          defaultArgs <- parseDefaultParams h
          return (False, expDomArgList, defaultArgs),
        return (True, (SE.emptySeries (Just SE.Paren) SE.Comma, []), (SE.emptySeries (Just SE.Bracket) SE.Comma, []))
      ]
  lift $ ensureArgumentLinearity S.empty $ map (\(mx, x, _, _, _) -> (mx, x)) $ SE.extract expSeries
  m <- getCurrentHint
  (c2, (cod, c)) <- parseDefInfoCod h m
  return (RT.RawGeist {loc, name = (name', c1), isConstLike, impArgs, defaultArgs, expArgs, cod = (c2, cod)}, c)

parseAliasGeist :: Handle -> Parser (a, C) -> Parser (RT.RawGeist a, C)
parseAliasGeist h nameParser = do
  loc <- getCurrentHint
  (name', c1) <- nameParser
  impArgs <- parseImplicitParams h
  (isConstLike, expArgs@(expSeries, _), defaultArgs) <- do
    choice
      [ do
          expDomArgList <- seriesParen $ preBinder h
          defaultArgs <- parseDefaultParams h
          return (False, expDomArgList, defaultArgs),
        return (True, (SE.emptySeries (Just SE.Paren) SE.Comma, []), (SE.emptySeries (Just SE.Bracket) SE.Comma, []))
      ]
  lift $ ensureArgumentLinearity S.empty $ map (\(mx, x, _, _, _) -> (mx, x)) $ SE.extract expSeries
  m <- getCurrentHint
  let cod = m :< RT.Tau
  return (RT.RawGeist {loc, name = (name', c1), isConstLike, impArgs, defaultArgs, expArgs, cod = ([], cod)}, [])

parseImplicitParams :: Handle -> Parser (SE.Series (RawBinder RT.RawType), C)
parseImplicitParams h =
  choice
    [ do
        (s, c) <- seriesAngle $ parseImplicitParam h
        return (s, c),
      return (SE.emptySeries (Just SE.Angle) SE.Comma, [])
    ]

parseDefaultParams :: Handle -> Parser (SE.Series (RawBinder RT.RawType, RT.RawTerm), C)
parseDefaultParams h =
  choice
    [ do
        (s, c) <- seriesBracket $ preBinderWithDefault h
        return (s, c),
      return (SE.emptySeries (Just SE.Bracket) SE.Comma, [])
    ]

parseImplicitArgsMaybe :: Handle -> Parser (Maybe (SE.Series RT.RawType), C)
parseImplicitArgsMaybe h =
  choice
    [ do
        (s, c) <- seriesAngle $ rawType h
        return (Just s, c),
      return (Nothing, [])
    ]

parseImplicitParam :: Handle -> Parser (RawBinder RT.RawType, C)
parseImplicitParam h = do
  ((m, x), varC) <- var h
  choice
    [ do
        c1 <- delimiter ":"
        (a, c2) <- rawType h
        return ((m, x, varC, c1, a), c2),
      do
        hole <- liftIO $ RT.createTypeHole (gensymHandle h) m
        return ((m, x, varC, [], hole), [])
    ]

ensureArgumentLinearity :: S.Set RawIdent -> [(Hint, RawIdent)] -> App ()
ensureArgumentLinearity foundVarSet vs =
  case vs of
    [] ->
      return ()
    (m, name) : rest
      | S.member name foundVarSet ->
          raiseError m $ "Found a non-linear occurrence of `" <> name <> "`."
      | otherwise ->
          ensureArgumentLinearity (S.insert name foundVarSet) rest

parseDefInfoCod :: Handle -> Hint -> Parser (C, (RT.RawType, C))
parseDefInfoCod h m =
  choice
    [ do
        c <- delimiter ":"
        t <- rawType h
        return (c, t),
      do
        hole <- liftIO $ RT.createTypeHole (gensymHandle h) m
        return ([], (hole, []))
    ]

rawTermDefine :: Handle -> O.Opacity -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermDefine h opacity m c0 = do
  (defInfo, c) <- parseDef h $ do
    (name, c1) <- baseName
    name' <- liftIO $ adjustHoleVar h name
    return (name', c1)
  return (m :< RT.PiIntroFix opacity c0 defInfo, c)

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
      rawTermMagicGlobal h m c,
      rawTermMagicCallType h m c,
      rawTermMagicGetTypeTag h m c,
      rawTermMagicGetDataArgs h m c,
      rawTermMagicGetConsSize h m c,
      rawTermMagicGetWrapperContentType h m c,
      rawTermMagicGetVectorContentType h m c,
      rawTermMagicGetNoemaContentType h m c,
      rawTermMagicGetBoxContentType h m c,
      rawTermMagicGetConstructorArgTypes h m c,
      rawTermMagicGetConsName h m c,
      rawTermMagicGetConsConstFlag h m c,
      rawTermMagicShowType h m c,
      rawTermMagicTextCons h m c,
      rawTermMagicTextUncons h m c,
      rawTermMagicCompileError h m c
    ]

rawTermMagicBase :: T.Text -> Parser (C -> C -> a) -> Parser (a, C)
rawTermMagicBase k parser = do
  c1 <- keyword k
  (c2, (magicF, c3)) <- betweenParen parser
  return (magicF c1 c2, c3)

rawTermMagicCast :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicCast h m c = do
  rawTermMagicBase "cast" $ do
    castFrom <- rawType h
    c3 <- delimiter ","
    castTo <- rawType h
    c4 <- delimiter ","
    value <- rawTerm h
    c6 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Cast c1 (c2, castFrom) (c3, castTo) (c4, value) c6)

rawTermMagicStore :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicStore h m c = do
  rawTermMagicBase "store" $ do
    t <- rawType h
    c3 <- delimiter ","
    value <- rawTerm h
    c4 <- delimiter ","
    pointer <- rawTerm h
    c5 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Store c1 (c2, t) (c3, value) (c4, pointer) c5)

rawTermMagicLoad :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicLoad h m c = do
  rawTermMagicBase "load" $ do
    t <- rawType h
    c3 <- delimiter ","
    pointer <- rawTerm h
    c4 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Load c1 (c2, t) (c3, pointer) c4)

rawTermMagicAlloca :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicAlloca h m c = do
  rawTermMagicBase "alloca" $ do
    t <- rawType h
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
  (t, c) <- rawType h
  return ((m, e, c1, c2, t), c)

rawTermMagicGlobal :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGlobal h m c = do
  rawTermMagicBase "global" $ do
    (globalVarName, c3) <- string
    c4 <- delimiter ","
    lt <- rawType h
    c5 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Global c1 (c2, (EN.ExternalName globalVarName, c3)) (c4, lt) c5)

rawTermMagicOpaqueValue :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicOpaqueValue h m c0 = do
  c1 <- keyword "opaque-value"
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.Magic c0 (RT.OpaqueValue c1 (c2, e)), c)

rawTermMagicCallType :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicCallType h m c = do
  rawTermMagicBase "call-type" $ do
    func <- rawTerm h
    c3 <- delimiter ","
    arg1 <- rawTerm h
    c4 <- delimiter ","
    arg2 <- rawTerm h
    return $ \c1 c2 -> m :< RT.Magic c (RT.CallType c1 (c2, func) (c3, arg1) (c4, arg2))

rawTermMagicGetTypeTag :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGetTypeTag h m c = do
  rawTermMagicBase "get-type-tag" $ do
    typeExpr <- rawType h
    return $ \_ c2 -> m :< RT.Magic c (RT.GetTypeTag (c2, typeExpr))

rawTermMagicGetDataArgs :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGetDataArgs h m c = do
  rawTermMagicBase "get-data-args" $ do
    typeExpr <- rawType h
    return $ \c1 c2 -> m :< RT.Magic c (RT.GetDataArgs c1 (c2, typeExpr))

rawTermMagicGetConsSize :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGetConsSize h m c = do
  rawTermMagicBase "get-cons-size" $ do
    typeExpr <- rawType h
    return $ \c1 c2 -> m :< RT.Magic c (RT.GetConsSize c1 (c2, typeExpr))

rawTermMagicGetWrapperContentType :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGetWrapperContentType h m c = do
  rawTermMagicBase "get-wrapper-content-type" $ do
    typeExpr <- rawType h
    return $ \c1 c2 -> m :< RT.Magic c (RT.GetWrapperContentType c1 (c2, typeExpr))

rawTermMagicGetVectorContentType :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGetVectorContentType h m c = do
  rawTermMagicBase "get-vector-content-type" $ do
    typeExpr <- rawType h
    return $ \c1 c2 -> m :< RT.Magic c (RT.GetVectorContentType c1 (c2, typeExpr))

rawTermMagicGetNoemaContentType :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGetNoemaContentType h m c = do
  rawTermMagicBase "get-noema-content-type" $ do
    typeExpr <- rawType h
    return $ \c1 c2 -> m :< RT.Magic c (RT.GetNoemaContentType c1 (c2, typeExpr))

rawTermMagicGetBoxContentType :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGetBoxContentType h m c = do
  rawTermMagicBase "get-box-content-type" $ do
    typeExpr <- rawType h
    return $ \c1 c2 -> m :< RT.Magic c (RT.GetBoxContentType c1 (c2, typeExpr))

rawTermMagicGetConstructorArgTypes :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGetConstructorArgTypes h m c = do
  rawTermMagicBase "get-constructor-arg-types" $ do
    typeExpr <- rawType h
    c3 <- delimiter ","
    index <- rawTerm h
    return $ \c1 c2 -> m :< RT.Magic c (RT.GetConstructorArgTypes c1 (c2, typeExpr) c3 (c3, index))

rawTermMagicGetConsName :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGetConsName h m c = do
  rawTermMagicBase "get-cons-name" $ do
    typeExpr <- rawType h
    c3 <- delimiter ","
    index <- rawTerm h
    return $ \c1 c2 -> m :< RT.Magic c (RT.GetConsArgName c1 (c2, typeExpr) c3 (c3, index))

rawTermMagicGetConsConstFlag :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGetConsConstFlag h m c = do
  rawTermMagicBase "get-cons-const-flag" $ do
    typeExpr <- rawType h
    c3 <- delimiter ","
    index <- rawTerm h
    return $ \c1 c2 -> m :< RT.Magic c (RT.GetConsConstFlag c1 (c2, typeExpr) c3 (c3, index))

rawTermMagicShowType :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicShowType h m c = do
  rawTermMagicBase "show-type" $ do
    typeExpr <- rawType h
    return $ \c1 c2 -> m :< RT.Magic c (RT.ShowType c1 (c2, typeExpr))

rawTermMagicTextCons :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicTextCons h m c = do
  rawTermMagicBase "text-cons" $ do
    runeTerm <- rawTerm h
    c3 <- delimiter ","
    textTerm <- rawTerm h
    return $ \c1 c2 -> m :< RT.Magic c (RT.TextCons c1 (c2, runeTerm) (c3, textTerm))

rawTermMagicTextUncons :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicTextUncons h m c = do
  rawTermMagicBase "text-uncons" $ do
    textTerm <- rawTerm h
    return $ \c1 c2 -> m :< RT.Magic c (RT.TextUncons c1 (c2, textTerm))

rawTermMagicCompileError :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicCompileError h m c = do
  rawTermMagicBase "compile-error" $ do
    msgTerm <- rawTerm h
    return $ \c1 c2 -> m :< RT.Magic c (RT.CompileError c1 (c2, msgTerm))

rawTermMatch :: Handle -> Hint -> C -> Bool -> Parser (RT.RawTerm, C)
rawTermMatch h m c1 isNoetic = do
  es <- bareSeries SE.Comma $ rawTerm h
  (patternRowList, c) <- seriesBraceList $ rawTermPatternRow h (length $ SE.extract es)
  return (m :< RT.DataElim c1 isNoetic es patternRowList, c)

rawTermPatternRow :: Handle -> Int -> Parser (RP.RawPatternRow RT.RawTerm, C)
rawTermPatternRow h patternSize = do
  m <- getCurrentHint
  patternList <- bareSeries SE.Comma $ rawTermPattern h
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
  m <- getCurrentHint
  (headSymbol, c) <- symbol'
  headSymbol' <-
    if headSymbol /= "_"
      then return headSymbol
      else liftIO $ newTextForHole (gensymHandle h)
  rawTermPatternBasic h m headSymbol' c

rawTermPatternBasic :: Handle -> Hint -> T.Text -> C -> Parser ((Hint, RP.RawPattern), C)
rawTermPatternBasic h m headSymbol c = do
  if T.null headSymbol
    then rawTermPatternRuneIntro m c
    else rawTermPatternConsOrVar h m headSymbol c

rawTermPatternRuneIntro :: Hint -> C -> Parser ((Hint, RP.RawPattern), C)
rawTermPatternRuneIntro m c1 = do
  (s, c2) <- rune
  case RU.make s of
    Right r ->
      return ((m, RP.RuneIntro r), c1 ++ c2)
    Left e ->
      lift $ raiseError m e

rawTermPatternConsOrVar :: Handle -> Hint -> T.Text -> C -> Parser ((Hint, RP.RawPattern), C)
rawTermPatternConsOrVar h m headSymbol c1 = do
  varOrLocator <- interpretVarName m headSymbol
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
        c2 <- delimiter ":="
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

rawTermBoxIntro :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermBoxIntro h m c1 = do
  vs <- bareSeries SE.Comma $ rawTermNoeticVar h
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.BoxIntro c1 c2 vs e, c)

rawTermBoxIntroLift :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermBoxIntroLift h m c1 = do
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.BoxIntroLift c1 c2 e, c)

rawTermCodeIntro :: Handle -> RT.CodeVariant -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermCodeIntro h codeVariant m c1 = do
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.CodeIntro codeVariant c1 c2 e, c)

rawTermTauIntro :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermTauIntro h m c1 = do
  (c2, (e, c)) <- betweenBrace $ rawType h
  return (m :< RT.TauIntro c1 (c2, e), c)

rawTermCodeElim :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermCodeElim h m c1 = do
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.CodeElim c1 c2 e, c)

rawTypeBox :: Handle -> Parser (RT.RawType, C)
rawTypeBox h = do
  m <- getCurrentHint
  c1 <- delimiter "+"
  (t, c) <- rawType h
  return (m :< RT.Box t, c1 ++ c)

rawTypeBoxNoema :: Handle -> Parser (RT.RawType, C)
rawTypeBoxNoema h = do
  m <- getCurrentHint
  c1 <- delimiter "&"
  (t, c) <- rawType h
  return (m :< RT.BoxNoema t, c1 ++ c)

rawTypeCode :: Handle -> Parser (RT.RawType, C)
rawTypeCode h = do
  m <- getCurrentHint
  c1 <- delimiter "'"
  (t, c) <- rawType h
  return (m :< RT.Code t, c1 ++ c)

rawTermFlowIntro :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermFlowIntro h m c1 = do
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.Detach c1 c2 e, c)

rawTermFlowElim :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermFlowElim h m c1 = do
  (c2, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.Attach c1 c2 e, c)

rawTypeOption :: Handle -> Parser (RT.RawType, C)
rawTypeOption h = do
  m <- getCurrentHint
  c1 <- delimiter "?"
  (t, c) <- rawType h
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
  series (Just (" of ", c1)) SE.Brace SE.Comma p

metaPiElim :: Parser (a, C) -> Parser (SE.Series a, C)
metaPiElim p = do
  c1 <- delimiter "::"
  series (Just ("::", c1)) SE.Paren SE.Comma p

foldPiElim ::
  Hint ->
  (RT.RawTerm, C) ->
  [PiElimArgList] ->
  (RT.RawTerm, C)
foldPiElim m (e, c) argListList =
  case argListList of
    [] ->
      (e, c)
    ((mImpArgs, c2, expArgs, c3, mDefaultArgs), c1) : rest ->
      foldPiElim m (m :< RT.PiElim e c mImpArgs c2 expArgs c3 mDefaultArgs, c1) rest

foldTyApp ::
  Hint ->
  (RT.RawType, C) ->
  [(SE.Series RT.RawType, C)] ->
  (RT.RawType, C)
foldTyApp m (t, c) argListList =
  case argListList of
    [] ->
      (t, c)
    (args, c1) : rest ->
      foldTyApp m (m :< RT.TyApp t c args, c1) rest

preBinder :: Handle -> Parser (RawBinder RT.RawType, C)
preBinder h = do
  mxc <- var h
  choice
    [ preAscription h mxc,
      preAscription' h mxc
    ]

preBinderWithDefault :: Handle -> Parser ((RawBinder RT.RawType, RT.RawTerm), C)
preBinderWithDefault h = do
  ((m, x), varC) <- var h
  choice
    [ do
        c2 <- delimiter ":="
        (defaultValue, c3) <- rawTerm h
        hole <- liftIO $ RT.createTypeHole (gensymHandle h) m
        let binder = (m, x, varC, [], hole)
        return ((binder, defaultValue), c2 ++ c3),
      do
        c1 <- delimiter ":"
        (a, c2) <- rawType h
        c3 <- delimiter ":="
        (defaultValue, c4) <- rawTerm h
        let binder = (m, x, varC, c1, a)
        return ((binder, defaultValue), c2 ++ c3 ++ c4)
    ]

preAscription :: Handle -> ((Hint, T.Text), C) -> Parser (RawBinder RT.RawType, C)
preAscription h ((m, x), c1) = do
  c2 <- delimiter ":"
  (a, c) <- rawType h
  return ((m, x, c1, c2, a), c)

preAscription' :: Handle -> ((Hint, T.Text), C) -> Parser (RawBinder RT.RawType, C)
preAscription' h ((m, x), c) = do
  hole <- liftIO $ RT.createTypeHole (gensymHandle h) m
  return ((m, x, c, [], hole), [])

typeWithoutIdent :: Handle -> Parser (RawBinder RT.RawType, C)
typeWithoutIdent h = do
  m <- getCurrentHint
  x <- liftIO $ newTextForHole (gensymHandle h)
  (t, c) <- rawType h
  return ((m, x, [], [], t), c)

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

interpretTypeName :: Hint -> T.Text -> Parser Name
interpretTypeName m varText = do
  case DD.getLocatorPair m varText of
    Left _ ->
      return (Var varText)
    Right (gl, ll) ->
      return (Locator (gl, ll))

rawTermTextIntro :: Parser (RT.RawTerm, C)
rawTermTextIntro = do
  m <- getCurrentHint
  (s, c) <- string
  textType <- lift $ locatorToTypeVar (blur m) coreText
  return (m :< RT.StaticText textType s, c)

rawTypeRune :: Hint -> C -> Parser (RT.RawType, C)
rawTypeRune m c = do
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

locatorToVarGlobal :: Hint -> T.Text -> App RT.RawTerm
locatorToVarGlobal m text = do
  (gl, ll) <- liftEither $ DD.getLocatorPair (blur m) text
  return $ rawVar (blur m) (Locator (gl, ll))

locatorToTypeVar :: Hint -> T.Text -> App RT.RawType
locatorToTypeVar m text = do
  (gl, ll) <- liftEither $ DD.getLocatorPair (blur m) text
  return $ blur m :< RT.TyVar (Locator (gl, ll))

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
