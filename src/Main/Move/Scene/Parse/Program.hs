module Main.Move.Scene.Parse.Program
  ( parseProgram,
    parseImport,
  )
where

import BaseParser.Move.GetInfo
import BaseParser.Rule.Parser
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Common.Move.Raise (raiseError)
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.ForeignCodType qualified as F
import Language.Common.Rule.LocalLocator qualified as LL
import Language.Common.Rule.Opacity qualified as O
import Language.Common.Rule.StmtKind qualified as SK
import Language.RawTerm.Rule.Name
import Language.RawTerm.Rule.RawBinder
import Language.RawTerm.Rule.RawStmt
import Language.RawTerm.Rule.RawTerm qualified as RT
import Logger.Rule.Hint
import Main.Move.Scene.Parse.Core qualified as P
import Main.Move.Scene.Parse.RawTerm
import SyntaxTree.Rule.C
import SyntaxTree.Rule.Series qualified as SE
import Text.Megaparsec

parseProgram :: Handle -> P.Parser RawProgram
parseProgram h = do
  m <- getCurrentHint
  importList <- parseImport
  stmtList <- many $ parseStmt h
  return $ RawProgram m importList stmtList

parseImport :: P.Parser [(RawImport, C)]
parseImport =
  many parseSingleImport

parseSingleImport :: P.Parser (RawImport, C)
parseSingleImport = do
  c1 <- P.keyword "import"
  m <- getCurrentHint
  (importItems, loc, c) <- P.seriesBrace' $ do
    mImportItem <- getCurrentHint
    locator <- P.symbol
    case fst locator of
      "static" -> do
        (ks, c) <- parseStaticKeyList
        return (RawStaticKey m c1 ks, c)
      _ -> do
        (lls, c) <- parseLocalLocatorList'
        return (RawImportItem mImportItem locator lls, c)
  return (RawImport c1 m importItems loc, c)

parseStmt :: Handle -> P.Parser (RawStmt, C)
parseStmt h = do
  choice
    [ parseDefine h,
      parseData h,
      parseInline h,
      parseNominal h,
      parseResource h,
      parseForeign h
    ]

parseLocalLocatorList' :: P.Parser (SE.Series (Hint, LL.LocalLocator), C)
parseLocalLocatorList' = do
  choice
    [ P.seriesBrace parseLocalLocator,
      return (SE.emptySeries (Just SE.Brace) SE.Comma, [])
    ]

parseStaticKeyList :: P.Parser (SE.Series (Hint, T.Text), C)
parseStaticKeyList = do
  choice
    [ P.seriesBrace $ do
        m <- getCurrentHint
        (k, c) <- P.symbol
        return ((m, k), c),
      return (SE.emptySeries (Just SE.Brace) SE.Comma, [])
    ]

parseLocalLocator :: P.Parser ((Hint, LL.LocalLocator), C)
parseLocalLocator = do
  m <- getCurrentHint
  (ll, c) <- P.baseName
  return ((m, LL.new ll), c)

parseForeign :: Handle -> P.Parser (RawStmt, C)
parseForeign h = do
  c1 <- P.keyword "foreign"
  (val, c) <- P.seriesBrace $ parseForeignItem h
  return (RawStmtForeign c1 val, c)

parseForeignItem :: Handle -> P.Parser (RawForeignItem, C)
parseForeignItem h = do
  m <- getCurrentHint
  (funcName, c1) <- P.symbol
  (domList, c2) <- P.seriesParen $ rawTerm h
  c3 <- P.delimiter ":"
  (cod, c) <-
    choice
      [ do
          c <- P.keyword "void"
          return (F.Void, c),
        do
          (lt, c) <- rawTerm h
          return (F.Cod lt, c)
      ]
  return (RawForeignItemF m (EN.ExternalName funcName) c1 domList c2 c3 cod, c)

parseDefine :: Handle -> P.Parser (RawStmt, C)
parseDefine h =
  parseDefine' h O.Opaque

parseInline :: Handle -> P.Parser (RawStmt, C)
parseInline h =
  parseDefine' h O.Clear

parseDefine' :: Handle -> O.Opacity -> P.Parser (RawStmt, C)
parseDefine' h opacity = do
  c1 <-
    case opacity of
      O.Opaque ->
        P.keyword "define"
      O.Clear ->
        P.keyword "inline"
  (def, c) <- parseDef h P.baseName
  return (RawStmtDefine c1 (SK.Normal opacity) def, c)

parseData :: Handle -> P.Parser (RawStmt, C)
parseData h = do
  c1 <- P.keyword "data"
  m <- getCurrentHint
  (dataName, c2) <- P.baseName
  dataArgsOrNone <- parseDataArgs h
  (consSeries, loc, c) <- P.seriesBraceList' $ parseDefineDataClause h
  return (RawStmtDefineData c1 m (dataName, c2) dataArgsOrNone consSeries loc, c)

parseNominal :: Handle -> P.Parser (RawStmt, C)
parseNominal h = do
  c1 <- P.keyword "nominal"
  m <- getCurrentHint
  (geists, c) <- P.seriesBrace $ do
    (geist, c) <- parseGeist h P.baseName
    loc <- getCurrentLoc
    return ((geist, loc), c)
  return (RawStmtNominal c1 m geists, c)

parseDataArgs :: Handle -> P.Parser (Maybe (RT.Args RT.RawTerm))
parseDataArgs h = do
  choice
    [ Just <$> try (P.seriesParen $ preBinder h),
      return Nothing
    ]

parseDefineDataClause :: Handle -> P.Parser (RawConsInfo BN.BaseName, C)
parseDefineDataClause h = do
  m <- getCurrentHint
  (consName, c1) <- P.baseName
  unless (isConsName (BN.reify consName)) $ do
    lift $ raiseError m "The name of a constructor must be capitalized"
  (consArgsOrNone, loc, c2) <- parseConsArgs h
  let consArgs = fromMaybe SE.emptySeriesPC consArgsOrNone
  let isConstLike = isNothing consArgsOrNone
  return ((m, consName, isConstLike, consArgs, loc), c1 ++ c2)

parseConsArgs :: Handle -> P.Parser (Maybe (SE.Series (RawBinder RT.RawTerm)), Loc, C)
parseConsArgs h = do
  choice
    [ do
        (series, loc, c) <- P.seriesParen' $ parseDefineDataClauseArg h
        return (Just series, loc, c),
      do
        loc <- getCurrentLoc
        return (Nothing, loc, [])
    ]

parseDefineDataClauseArg :: Handle -> P.Parser (RawBinder RT.RawTerm, C)
parseDefineDataClauseArg h = do
  choice
    [ try $ var h >>= preAscription h,
      typeWithoutIdent h
    ]

parseResource :: Handle -> P.Parser (RawStmt, C)
parseResource h = do
  c1 <- P.keyword "resource"
  m <- getCurrentHint
  (name, c2) <- P.baseName
  (handlers, c) <- P.seriesBrace $ rawExpr h
  case SE.elems handlers of
    [discarder, copier] -> do
      return (RawStmtDefineResource c1 m (name, c2) discarder copier (SE.trailingComment handlers), c)
    _ ->
      failure Nothing (S.fromList [asLabel "discarder and copier"])
