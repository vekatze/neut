module Kernel.Parse.Internal.RawTerm
  ( Handle (..),
    new,
    rawExpr,
    rawTerm,
    rawType,
    var,
    varWithMode,
    preAscription,
    preBinder,
    ArrowMode (..),
    mandatoryBinder,
    betweenBrace',
    interpretVarName,
    parseDef,
    parseAliasDef,
    parseGeist,
    parseNominalGeist,
    parseAliasGeist,
    parseResourceGeist,
    parseConstantDef,
    parseConstantGeist,
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
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Gensym.Handle qualified as Gensym
import Kernel.Common.Const
import Kernel.Parse.Internal.Util (isNumericLike)
import Language.Common.BaseName qualified as BN
import Language.Common.CreateSymbol (newTextForHole)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.LocalDefKind qualified as LDK
import Language.Common.Rune qualified as RU
import Language.Common.VarKind qualified as VK
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
            "invoke" ->
              rawTermInvoke h m c
            "unpack-type" ->
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

data TermMode
  = Full
  | Partial

rawTerm :: Handle -> Parser (RT.RawTerm, C)
rawTerm h = do
  m <- getCurrentHint
  (headSymbol, c) <- symbol'
  rawTerm' h m headSymbol c

rawTermPartial :: Handle -> Parser (RT.RawTerm, C)
rawTermPartial h = do
  m <- getCurrentHint
  (headSymbol, c) <- symbol'
  rawTermBase Partial h m headSymbol c

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
    _ -> do
      (nameText, c') <- extendLocatorSuffix headSymbol c
      if T.null nameText
        then do
          choice
            [ rawTypeBrace h,
              rawTypePi h,
              rawTypeBox h,
              rawTypeBoxNoema h,
              rawTypeCode h,
              rawTypeOption h
            ]
        else do
          name <- interpretTypeName m nameText
          rawTypeTyAppCont h (m :< RT.TyVar name, c')

rawTerm' :: Handle -> Hint -> T.Text -> C -> Parser (RT.RawTerm, C)
rawTerm' h m headSymbol c = do
  case headSymbol of
    "define" -> do
      rawTermDefine h LDK.Define m c
    "inline" -> do
      rawTermDefine h LDK.Inline m c
    "define-meta" -> do
      rawTermDefine h LDK.DefineMeta m c
    "inline-meta" -> do
      rawTermDefine h LDK.InlineMeta m c
    "introspect" -> do
      rawTermIntrospect h m c
    "static" -> do
      rawTermStatic m c
    "magic" -> do
      rawTermMagic h m c
    "match" -> do
      rawTermMatch h m c False
    "case" -> do
      rawTermMatch h m c True
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
    "pack-type" -> do
      rawTermTauIntro h m c
    "exact" -> do
      rawTermPiElimExact h m c
    "if" -> do
      rawTermIf h m c
    "when" -> do
      rawTermWhen h m c
    "with" -> do
      rawTermWith h m c
    "admit" -> do
      rawTermAdmit m c
    _ -> do
      rawTermBase Full h m headSymbol c

rawTermInvoke :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermInvoke h m c1 = do
  tropes <- bareSeries SE.Comma $ do
    mTrope <- getCurrentHint
    (nameText, cName) <- symbolWithLocatorSuffix
    name <- interpretVarName mTrope nameText
    return ((mTrope, name), cName)
  c2 <- delimiter ";"
  (body, c) <- rawExpr h
  return (m :< RT.Invoke c1 tropes c2 body, c)

rawTermBase :: TermMode -> Handle -> Hint -> T.Text -> C -> Parser (RT.RawTerm, C)
rawTermBase mode h m headSymbol c = do
  (nameText, nameC, hasMetaDelimiter) <- extendTermLocatorSuffix headSymbol c
  if T.null nameText
    then do
      let parsers =
            [ rawTermLambda h,
              rawTermTextIntro,
              rawTermRuneIntro,
              rawTermEmbody h
            ]
      case mode of
        Partial ->
          choice parsers
        Full ->
          choice $ rawTermBrace h : parsers
    else do
      name <- interpretVarName m nameText
      (mImpArgs, cImpArgs) <- parseImplicitArgsMaybe h
      if hasMetaDelimiter
        then
          choice
            [ do
                (es, cArgs) <- seriesParen $ rawTerm h
                mDefaultArgs <- optional $ seriesBracket $ rawTermKeyValuePair h
                case mDefaultArgs of
                  Nothing ->
                    return (m :< RT.PiElimMeta name nameC mImpArgs cImpArgs es [] Nothing, cArgs)
                  Just (defaultArgs, cDefaultArgs) ->
                    return (m :< RT.PiElimMeta name nameC mImpArgs cImpArgs es cArgs (Just defaultArgs), cDefaultArgs),
              do
                (es, cArgs) <- seriesBracket $ rawTerm h
                case mImpArgs of
                  Nothing ->
                    return (m :< RT.PiElimRule name nameC es, cArgs)
                  Just _ ->
                    lift $ raiseError m "Rule application cannot take implicit type arguments",
              do
                (kvs, cArgs) <- keyValueArgs $ rawTermKeyValuePair h
                return (m :< RT.PiElimMetaByKey name nameC mImpArgs cImpArgs kvs, cArgs)
            ]
        else do
          let parseByKey = do
                (fieldsWithRest, cArgs) <- keyValueArgs $ rawTermKeyValuePair h
                (fields, restArg) <- lift $ extractRestArg fieldsWithRest
                return (m :< RT.PiElimByKey name nameC mImpArgs cImpArgs fields restArg, cArgs)
          let parseCont =
                rawTermPiElimContWithImp h m name nameC mImpArgs cImpArgs
          case mode of
            Full ->
              choice [parseByKey, parseCont]
            Partial ->
              parseCont

rawTermPiElimCont :: Handle -> (RT.RawTerm, C) -> Parser (RT.RawTerm, C)
rawTermPiElimCont h (e@(m :< _), c) = do
  argListList <- many $ rawTermPiElimArgs h
  return $ foldPiElim m (e, c) argListList

rawTermPiElimContWithImp ::
  Handle ->
  Hint ->
  Name ->
  C ->
  Maybe (SE.Series RT.RawType) ->
  C ->
  Parser (RT.RawTerm, C)
rawTermPiElimContWithImp h m name c mImpArgs cImpArgs = do
  let e = m :< RT.Var name
  case mImpArgs of
    Nothing ->
      rawTermPiElimCont h (e, c)
    Just impArgs -> do
      choice
        [ do
            (expArgs, c2) <- seriesParen (rawTerm h)
            mDefaultArgs <- optional $ seriesBracket $ rawTermKeyValuePair h
            let (args, c3) = case mDefaultArgs of
                  Nothing ->
                    ((Just impArgs, cImpArgs, expArgs, c2, Nothing), [])
                  Just (defaultArgs, c4) ->
                    ((Just impArgs, cImpArgs, expArgs, c2, Just defaultArgs), c4)
            rest <- many $ rawTermPiElimArgs h
            return $ foldPiElim m (e, c) ((args, c3) : rest),
          do
            return (m :< RT.PiElimImplicit name cImpArgs impArgs, [])
        ]

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
  expArgs <- seriesParen (choice [try $ varWithMode h >>= preAscription h, typeWithoutIdent h])
  defaultArgs <- parseDefaultTypeParams h
  (piKind, cArrow) <-
    choice
      [ do
          cArrow <- delimiter "->>"
          return (RT.PiDestPass, cArrow),
        do
          cArrow <- delimiter "->"
          return (RT.PiNormal, cArrow)
      ]
  (cod, c) <- rawType h
  loc <- getCurrentLoc
  return (m :< RT.Pi impArgs expArgs defaultArgs piKind cArrow cod loc, c)

rawTermLambda :: Handle -> Parser (RT.RawTerm, C)
rawTermLambda h = do
  m <- getCurrentHint
  impArgs <- parseImplicitParams h
  expArgs@(expSeries, _) <- seriesParen $ preBinder h
  defaultArgs <- parseDefaultParams h
  lift $ ensureArgumentLinearity S.empty $ map (\(mx, _, x, _, _, _) -> (mx, x)) $ SE.extract expSeries
  cod <- liftIO $ RT.createTypeHole (gensymHandle h) m
  (isDestPassing, cArrow) <-
    choice
      [ do
          c' <- delimiter "=>>"
          return (True, c'),
        do
          c' <- delimiter "=>"
          return (False, c')
      ]
  (c2, ((e, c3), loc, c)) <- betweenBrace' $ rawExpr h
  let geist =
        RT.RawGeist
          { loc = m,
            name = (Nothing, []),
            isConstLike = False,
            isDestPassing,
            impArgs,
            defaultArgs,
            expArgs,
            cod = ([], cod)
          }
  let defInfo =
        RT.RawDef
          { geist,
            leadingComment = cArrow ++ c2,
            body = e,
            trailingComment = c3,
            endLoc = loc
          }
  return (m :< RT.PiIntro [] defInfo, c)

rawTermKeyValuePair :: Handle -> Parser ((Hint, Key, C, C, RT.RawTerm), C)
rawTermKeyValuePair h = do
  m <- getCurrentHint
  (key, c1) <- symbol
  choice
    [ do
        c2 <- delimiter ":="
        (value, c) <- rawTerm h
        return ((m, key, c1, c2, value), c),
      do
        return ((m, key, c1, [], m :< RT.Var (Var key)), [])
    ]

extractRestArg ::
  SE.Series (Hint, Key, C, C, RT.RawTerm) ->
  App (SE.Series (Hint, Key, C, C, RT.RawTerm), Maybe (Hint, C, C, RT.RawTerm))
extractRestArg fieldSeries =
  extractRestArg' fieldSeries (SE.elems fieldSeries) [] Nothing

extractRestArg' ::
  SE.Series (Hint, Key, C, C, RT.RawTerm) ->
  [(C, (Hint, Key, C, C, RT.RawTerm))] ->
  [(C, (Hint, Key, C, C, RT.RawTerm))] ->
  Maybe (Hint, C, C, RT.RawTerm) ->
  App (SE.Series (Hint, Key, C, C, RT.RawTerm), Maybe (Hint, C, C, RT.RawTerm))
extractRestArg' fieldSeries elems acc restArg = do
  case elems of
    [] ->
      return (fieldSeries {SE.elems = reverse acc}, restArg)
    (c, (m, "..", c1, c2, value)) : rest -> do
      case restArg of
        Just (_, _, _, _) ->
          raiseError m "The pseudo-field `..` can be specified at most once"
        Nothing ->
          extractRestArg' fieldSeries rest acc $ Just (m, c, c1 ++ c2, value)
    entry : rest ->
      extractRestArg' fieldSeries rest (entry : acc) restArg

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
  ((mx, k, x), c2) <- rawTermNoeticVar h
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
  return (m :< RT.Pin c1 (mx, k, x, c2, c3, t) c4 noeticVarList c5 e1 c6 loc c7 e2 endLoc, c)

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

ensureIdentLinearity :: S.Set RawIdent -> [(Hint, VK.VarKind, RawIdent)] -> App ()
ensureIdentLinearity foundVarSet vs =
  case vs of
    [] ->
      return ()
    (m, _, name) : rest
      | S.member name foundVarSet ->
          raiseError m $ "Found a non-linear occurrence of `" <> name <> "`."
      | otherwise ->
          ensureIdentLinearity (S.insert name foundVarSet) rest

rawTermNoeticVar :: Handle -> Parser ((Hint, VK.VarKind, T.Text), C)
rawTermNoeticVar h = do
  ((m, k, x), c) <- varWithMode h
  return ((m, k, x), c)

rawTermEmbody :: Handle -> Parser (RT.RawTerm, C)
rawTermEmbody h = do
  m <- getCurrentHint
  c1 <- delimiter "*"
  (e, c) <- rawTermPartial h
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

parseDef :: ArrowMode -> Handle -> Parser (a, C) -> Parser (RT.RawDef a, C)
parseDef arrowMode h nameParser = do
  (geist, c1) <- parseGeistWith arrowMode ParseDefaultArgs h nameParser
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

data DefaultArgsMode
  = ParseDefaultArgs
  | NoDefaultArgs

data ArrowMode
  = ArrowMeta
  | ArrowObject

parseGeist :: Handle -> Parser (a, C) -> Parser (RT.RawGeist a, C)
parseGeist =
  parseGeistWith ArrowObject ParseDefaultArgs

parseNominalGeist :: ArrowMode -> Handle -> Parser (a, C) -> Parser (RT.RawGeist a, C)
parseNominalGeist arrowMode =
  parseGeistWith arrowMode NoDefaultArgs

parseGeistWith :: ArrowMode -> DefaultArgsMode -> Handle -> Parser (a, C) -> Parser (RT.RawGeist a, C)
parseGeistWith arrowMode mode h nameParser = do
  loc <- getCurrentHint
  name <- nameParser
  impArgs <- parseImplicitParams h
  let isConstLike = False
  expArgs@(expSeries, _) <- seriesParen $ mandatoryBinder h
  defaultArgs <- case mode of
    ParseDefaultArgs -> parseDefaultParams h
    NoDefaultArgs -> return RT.emptyDefaultArgs
  lift $ ensureArgumentLinearity S.empty $ map (\(mx, _, x, _, _, _) -> (mx, x)) $ SE.extract expSeries
  (isDestPassing, c2, (cod, c)) <- parseDefInfoCod arrowMode h
  return (RT.RawGeist {loc, name, isConstLike, isDestPassing, impArgs, defaultArgs, expArgs, cod = (c2, cod)}, c)

parseAliasGeist :: Handle -> Parser (a, C) -> Parser (RT.RawGeist a, C)
parseAliasGeist h nameParser = do
  loc <- getCurrentHint
  (name', c1) <- nameParser
  impArgs <- parseImplicitParams h
  (isConstLike, expArgs@(expSeries, _)) <- do
    choice
      [ do
          expDomArgList <- seriesParen $ preBinder h
          return (False, expDomArgList),
        return (True, (SE.emptySeries (Just SE.Paren) SE.Comma, []))
      ]
  lift $ ensureArgumentLinearity S.empty $ map (\(mx, _, x, _, _, _) -> (mx, x)) $ SE.extract expSeries
  m <- getCurrentHint
  let cod = m :< RT.Tau
  let isDestPassing = False
  return (RT.RawGeist {loc, name = (name', c1), isConstLike, isDestPassing, impArgs, defaultArgs = RT.emptyDefaultArgs, expArgs, cod = ([], cod)}, [])

parseResourceGeist :: Parser (a, C) -> Parser (RT.RawGeist a, C)
parseResourceGeist nameParser = do
  loc <- getCurrentHint
  (name', c1) <- nameParser
  let impArgs = (SE.emptySeriesAC, [])
  let isConstLike = True
  let isDestPassing = False
  let expArgs = (SE.emptySeries (Just SE.Paren) SE.Comma, [])
  let defaultArgs = (SE.emptySeries (Just SE.Bracket) SE.Comma, [])
  let cod = loc :< RT.Tau
  return (RT.RawGeist {loc, name = (name', c1), isConstLike, isDestPassing, impArgs, defaultArgs, expArgs, cod = ([], cod)}, [])

parseConstantDef :: Handle -> Parser (a, C) -> Parser (RT.RawDef a, C)
parseConstantDef h nameParser = do
  (geist, c1) <- parseConstantGeist h nameParser
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

parseConstantGeist :: Handle -> Parser (a, C) -> Parser (RT.RawGeist a, C)
parseConstantGeist h nameParser = do
  loc <- getCurrentHint
  (name', c1) <- nameParser
  impArgs <- parseImplicitParams h
  let isConstLike = True
  let expArgs = (SE.emptySeries (Just SE.Paren) SE.Comma, [])
  let defaultArgs = (SE.emptySeries (Just SE.Bracket) SE.Comma, [])
  c2 <- delimiter ":"
  (cod, c) <- rawType h
  let isDestPassing = False
  return (RT.RawGeist {loc, name = (name', c1), isConstLike, isDestPassing, impArgs, defaultArgs, expArgs, cod = (c2, cod)}, c)

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

parseDefaultTypeParams :: Handle -> Parser (SE.Series (RawBinder RT.RawType), C)
parseDefaultTypeParams h =
  choice
    [ do
        (s, c) <- seriesBracket $ preBinder h
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
  ((m, k, x), varC) <- varWithMode h
  choice
    [ do
        c1 <- delimiter ":"
        (a, c2) <- rawType h
        return ((m, k, x, varC, c1, a), c2),
      do
        hole <- liftIO $ RT.createTypeHole (gensymHandle h) m
        return ((m, k, x, varC, [], hole), [])
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

parseDefInfoCod :: ArrowMode -> Handle -> Parser (Bool, C, (RT.RawType, C))
parseDefInfoCod arrowMode h = do
  (isDestPassing, c) <-
    case arrowMode of
      ArrowMeta -> do
        c <- delimiter "->"
        return (False, c)
      ArrowObject ->
        choice
          [ do
              c <- delimiter "->>"
              return (True, c),
            do
              c <- delimiter "->"
              return (False, c)
          ]
  t <- rawType h
  return (isDestPassing, c, t)

rawTermDefine :: Handle -> LDK.LocalDefKind -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermDefine h kind m c0 = do
  let arrowMode = case kind of
        LDK.DefineMeta -> ArrowMeta
        LDK.InlineMeta -> ArrowMeta
        _ -> ArrowObject
  (defInfo, c) <- parseDef arrowMode h $ do
    (name, c1) <- baseName
    name' <- liftIO $ adjustHoleVar h name
    return (name', c1)
  return (m :< RT.PiIntroFix kind c0 defInfo, c)

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
      rawTermMagicCalloc h m c,
      rawTermMagicMalloc h m c,
      rawTermMagicRealloc h m c,
      rawTermMagicFree h m c,
      rawTermMagicExternal h m c,
      rawTermMagicOpaqueValue h m c,
      rawTermMagicGlobal h m c,
      rawTermMagicCallType h m c,
      rawTermMagicInspectType h m c,
      rawTermMagicEqType h m c,
      rawTermMagicShowType h m c,
      rawTermMagicAssertMixable h m c,
      rawTermMagicTextCons h m c,
      rawTermMagicTextUncons h m c,
      rawTermMagicMakeSwitch h m c,
      rawTermMagicCompileError h m c,
      rawTermMagicGetOriginFileName h m c,
      rawTermMagicGetOriginLine h m c,
      rawTermMagicGetOriginColumn h m c
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

rawTermMagicCalloc :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicCalloc h m c = do
  rawTermMagicBase "calloc" $ do
    num <- rawTerm h
    c3 <- delimiter ","
    size <- rawTerm h
    c4 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Calloc c1 (c2, num) (c3, size) c4)

rawTermMagicMalloc :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicMalloc h m c = do
  rawTermMagicBase "malloc" $ do
    size <- rawTerm h
    c3 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Malloc c1 (c2, size) c3)

rawTermMagicRealloc :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicRealloc h m c = do
  rawTermMagicBase "realloc" $ do
    ptr <- rawTerm h
    c3 <- delimiter ","
    size <- rawTerm h
    c4 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Realloc c1 (c2, ptr) (c3, size) c4)

rawTermMagicFree :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicFree h m c = do
  rawTermMagicBase "free" $ do
    ptr <- rawTerm h
    c3 <- optional $ delimiter ","
    return $ \c1 c2 -> m :< RT.Magic c (RT.Free c1 (c2, ptr) c3)

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
    c5 <- delimiter ","
    arg3 <- rawTerm h
    return $ \c1 c2 -> m :< RT.Magic c (RT.CallType c1 (c2, func) (c3, arg1) (c4, arg2) (c5, arg3))

rawTermMagicInspectType :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicInspectType h m c = do
  rawTermMagicBase "inspect-type" $ do
    typeExpr <- rawType h
    return $ \_ c2 -> m :< RT.Magic c (RT.InspectType (c2, typeExpr))

rawTermMagicEqType :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicEqType h m c = do
  rawTermMagicBase "eq-type" $ do
    typeExpr1 <- rawType h
    c3 <- delimiter ","
    typeExpr2 <- rawType h
    return $ \_ c2 -> m :< RT.Magic c (RT.EqType (c2, typeExpr1) (c3, typeExpr2))

rawTermMagicShowType :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicShowType h m c = do
  rawTermMagicBase "show-type" $ do
    typeExpr <- rawType h
    return $ \c1 c2 -> m :< RT.Magic c (RT.ShowType c1 (c2, typeExpr))

rawTermMagicAssertMixable :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicAssertMixable h m c = do
  rawTermMagicBase "assert-mixable" $ do
    typeExpr <- rawType h
    return $ \c1 c2 -> m :< RT.Magic c (RT.AssertMixable c1 (c2, typeExpr))

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

rawTermMagicMakeSwitch :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicMakeSwitch h m c = do
  rawTermMagicBase "make-switch" $ do
    keyTerm <- rawTerm h
    c3 <- delimiter ","
    fallbackTerm <- rawTerm h
    c4 <- delimiter ","
    clausesTerm <- rawTerm h
    return $ \c1 c2 -> m :< RT.Magic c (RT.MakeSwitch c1 (c2, keyTerm) (c3, fallbackTerm) (c4, clausesTerm))

rawTermMagicCompileError :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicCompileError h m c = do
  rawTermMagicBase "compile-error" $ do
    msgTerm <- rawTerm h
    return $ \c1 c2 -> m :< RT.Magic c (RT.CompileError c1 (c2, msgTerm))

rawTermMagicGetOriginFileName :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGetOriginFileName _ m c = do
  rawTermMagicBase "get-origin-file-name" $ do
    return $ \c1 c2 -> m :< RT.Magic c (RT.GetOriginFileName c1 c2)

rawTermMagicGetOriginLine :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGetOriginLine _ m c = do
  rawTermMagicBase "get-origin-line" $ do
    return $ \c1 c2 -> m :< RT.Magic c (RT.GetOriginLine c1 c2)

rawTermMagicGetOriginColumn :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermMagicGetOriginColumn _ m c = do
  rawTermMagicBase "get-origin-column" $ do
    return $ \c1 c2 -> m :< RT.Magic c (RT.GetOriginColumn c1 c2)

rawTermMatch :: Handle -> Hint -> C -> Bool -> Parser (RT.RawTerm, C)
rawTermMatch h m c1 isNoetic = do
  es <- bareSeries SE.Comma $ rawTermPartial h
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
  mBang <- optional $ delimiter "!"
  m <- getCurrentHint
  (headSymbol, c) <- symbol'
  (headSymbol', c') <-
    if headSymbol == "_"
      then do
        hole <- liftIO $ newTextForHole (gensymHandle h)
        return (hole, c)
      else extendLocatorSuffix headSymbol c
  let k =
        case mBang of
          Just _ -> VK.Exp
          Nothing -> VK.Normal
  let c'' = fromMaybe [] mBang ++ c'
  rawTermPatternBasic h m k headSymbol' c''

rawTermPatternBasic :: Handle -> Hint -> VK.VarKind -> T.Text -> C -> Parser ((Hint, RP.RawPattern), C)
rawTermPatternBasic h m k headSymbol c = do
  if T.null headSymbol
    then rawTermPatternRuneIntro m c
    else rawTermPatternConsOrVar h m k headSymbol c

rawTermPatternRuneIntro :: Hint -> C -> Parser ((Hint, RP.RawPattern), C)
rawTermPatternRuneIntro m c1 = do
  (s, c2) <- rune
  case RU.make s of
    Right r ->
      return ((m, RP.RuneIntro r), c1 ++ c2)
    Left e ->
      lift $ raiseError m e

rawTermPatternConsOrVar :: Handle -> Hint -> VK.VarKind -> T.Text -> C -> Parser ((Hint, RP.RawPattern), C)
rawTermPatternConsOrVar h m k headSymbol c1 = do
  if k == VK.Exp
    then return ((m, RP.Var k (Var headSymbol)), c1)
    else do
      varOrLocator <- interpretVarName m headSymbol
      choice
        [ do
            (patArgs, c) <- seriesParen $ rawTermPattern h
            return ((m, RP.Cons varOrLocator c1 (RP.Paren patArgs)), c),
          do
            (kvs, c) <- keyValueArgs $ rawTermPatternKeyValuePair h
            return ((m, RP.Cons varOrLocator c1 (RP.Of kvs)), c),
          do
            return ((m, RP.Var k varOrLocator), c1)
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
        return ((from, (mFrom, [], RP.Var VK.Normal (Var from))), []) -- record rhyming
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
  cond <- rawTermPartial h
  (c2, (body, c3)) <- betweenBrace $ rawExpr h
  return (((c1, cond), (c2, body)), c3)

rawTermClause :: Handle -> C -> Parser (RT.KeywordClause RT.RawTerm, C)
rawTermClause h c1 = do
  cond <- rawTermPartial h
  (c2, (body, c3)) <- betweenBrace $ rawExpr h
  return (((c1, cond), (c2, body)), c3)

rawTermWhen :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermWhen h m c0 = do
  (whenClause, c) <- rawTermClause h c0
  return (m :< RT.When whenClause, c)

rawTermWith :: Handle -> Hint -> C -> Parser (RT.RawTerm, C)
rawTermWith h m c0 = do
  (withClause, c) <- rawTermClause h c0
  return (m :< RT.With withClause, c)

rawTermBrace :: Handle -> Parser (RT.RawTerm, C)
rawTermBrace h = do
  m <- getCurrentHint
  (c1, (e, c)) <- betweenBrace $ rawExpr h
  return (m :< RT.Brace c1 e, c)

rawTypeBrace :: Handle -> Parser (RT.RawType, C)
rawTypeBrace h = do
  m <- getCurrentHint
  (c1, (t, c)) <- betweenBrace $ rawType h
  return (m :< RT.TyBrace c1 t, c)

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

rawTypeOption :: Handle -> Parser (RT.RawType, C)
rawTypeOption h = do
  m <- getCurrentHint
  c1 <- delimiter "?"
  (t, c) <- rawType h
  return (m :< RT.Option t, c1 ++ c)

rawTermAdmit :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermAdmit m c = do
  return (m :< RT.Admit, c)

keyValueArgs :: Parser (a, C) -> Parser (SE.Series a, C)
keyValueArgs p = do
  series Nothing SE.Brace SE.Comma p

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
  mxc <- varWithMode h
  choice
    [ preAscription h mxc,
      preAscription' h mxc
    ]

mandatoryBinder :: Handle -> Parser (RawBinder RT.RawType, C)
mandatoryBinder h = do
  mxc <- varWithMode h
  preAscription h mxc

preBinderWithDefault :: Handle -> Parser ((RawBinder RT.RawType, RT.RawTerm), C)
preBinderWithDefault h = do
  ((m, k, x), varC) <- varWithMode h
  choice
    [ do
        c2 <- delimiter ":="
        (defaultValue, c3) <- rawTerm h
        hole <- liftIO $ RT.createTypeHole (gensymHandle h) m
        let binder = (m, k, x, varC, [], hole)
        return ((binder, defaultValue), c2 ++ c3),
      do
        c1 <- delimiter ":"
        (a, c2) <- rawType h
        c3 <- delimiter ":="
        (defaultValue, c4) <- rawTerm h
        let binder = (m, k, x, varC, c1, a)
        return ((binder, defaultValue), c2 ++ c3 ++ c4)
    ]

preAscription :: Handle -> ((Hint, VK.VarKind, T.Text), C) -> Parser (RawBinder RT.RawType, C)
preAscription h ((m, k, x), c1) = do
  c2 <- delimiter ":"
  (a, c) <- rawType h
  return ((m, k, x, c1, c2, a), c)

preAscription' :: Handle -> ((Hint, VK.VarKind, T.Text), C) -> Parser (RawBinder RT.RawType, C)
preAscription' h ((m, k, x), c) = do
  hole <- liftIO $ RT.createTypeHole (gensymHandle h) m
  return ((m, k, x, c, [], hole), [])

typeWithoutIdent :: Handle -> Parser (RawBinder RT.RawType, C)
typeWithoutIdent h = do
  m <- getCurrentHint
  x <- liftIO $ newTextForHole (gensymHandle h)
  (t, c) <- rawType h
  return ((m, VK.Normal, x, [], [], t), c)

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

rawTermStatic :: Hint -> C -> Parser (RT.RawTerm, C)
rawTermStatic m c1 = do
  mKey <- getCurrentHint
  (key, c) <- symbol
  return (m :< RT.Static c1 mKey (RT.StaticFileKey key), c)

interpretVarName :: Hint -> T.Text -> Parser Name
interpretVarName m varText = do
  case DD.getLocatorPairMaybe m varText of
    Left err
      | T.any (== ':') varText ->
          lift $ throwError err
      | otherwise ->
          return (Var varText)
    Right Nothing ->
      return (Var varText)
    Right (Just (gl, ll))
      | isNumericLike varText ->
          return (Var varText)
      | otherwise ->
          return (Locator (gl, ll))

interpretTypeName :: Hint -> T.Text -> Parser Name
interpretTypeName m varText = do
  case DD.getLocatorPairMaybe m varText of
    Left err
      | T.any (== ':') varText ->
          lift $ throwError err
      | otherwise ->
          return (Var varText)
    Right Nothing ->
      return (Var varText)
    Right (Just (gl, ll)) ->
      return (Locator (gl, ll))

rawTermTextIntro :: Parser (RT.RawTerm, C)
rawTermTextIntro = do
  m <- getCurrentHint
  (s, c) <- string
  return (m :< RT.String s, c)

rawTypeRune :: Hint -> C -> Parser (RT.RawType, C)
rawTypeRune m c = do
  return (m :< RT.Rune, c)

rawTermRuneIntro :: Parser (RT.RawTerm, C)
rawTermRuneIntro = do
  m <- getCurrentHint
  (s, c) <- rune
  case RU.make s of
    Right r ->
      return (m :< RT.RuneIntro r, c)
    Left e ->
      lift $ raiseError m e

symbolWithLocatorSuffix :: Parser (T.Text, C)
symbolWithLocatorSuffix = do
  (headSymbol, c) <- symbol
  extendLocatorSuffix headSymbol c

extendTermLocatorSuffix :: T.Text -> C -> Parser (T.Text, C, Bool)
extendTermLocatorSuffix headSymbol c = do
  mSep <- optional $ delimiter routeSep
  case mSep of
    Nothing ->
      return (headSymbol, c, False)
    Just cSep -> do
      (suffix, cSuffix) <- symbol'
      if T.null suffix
        then do
          return (headSymbol, c <> cSep <> cSuffix, True)
        else do
          (suffix', cRest, hasMetaDelimiter) <- extendTermLocatorSuffix suffix cSuffix
          return (headSymbol <> routeSep <> suffix', c <> cSep <> cRest, hasMetaDelimiter)

extendLocatorSuffix :: T.Text -> C -> Parser (T.Text, C)
extendLocatorSuffix headSymbol c = do
  mSep <- optional $ delimiter routeSep
  case mSep of
    Nothing ->
      return (headSymbol, c)
    Just cSep -> do
      (suffix, cSuffix) <- symbol
      (suffix', cRest) <- extendLocatorSuffix suffix cSuffix
      return (headSymbol <> routeSep <> suffix', c <> cSep <> cRest)

var :: Handle -> Parser ((Hint, T.Text), C)
var h = do
  m <- getCurrentHint
  (x, c) <- symbol
  if x /= "_"
    then return ((m, x), c)
    else do
      unusedVar <- liftIO $ newTextForHole (gensymHandle h)
      return ((m, unusedVar), c)

varWithMode :: Handle -> Parser ((Hint, VK.VarKind, T.Text), C)
varWithMode h = do
  mBang <- optional $ delimiter "!"
  m <- getCurrentHint
  (x, c) <- symbol
  let k =
        case mBang of
          Just _ ->
            VK.Exp
          Nothing ->
            VK.Normal
  let c' = fromMaybe [] mBang ++ c
  if x /= "_"
    then return ((m, k, x), c')
    else do
      unusedVar <- liftIO $ newTextForHole (gensymHandle h)
      return ((m, k, unusedVar), c')

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
