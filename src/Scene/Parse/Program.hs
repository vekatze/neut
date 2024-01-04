module Scene.Parse.Program
  ( parseProgram,
    parseImport,
  )
where

import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Entity.BaseName qualified as BN
import Entity.C
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Entity.Name
import Entity.Opacity qualified as O
import Entity.RawBinder
import Entity.RawProgram
import Entity.RawTerm qualified as RT
import Entity.StmtKind qualified as SK
import Entity.Syntax.Series qualified as SE
import Scene.Parse.Core qualified as P
import Scene.Parse.RawTerm
import Text.Megaparsec

parseProgram :: P.Parser RawProgram
parseProgram = do
  m <- P.getCurrentHint
  (importBlockOrNone, c1) <- parseImport
  stmtList <- many parseStmt
  return $ RawProgram m importBlockOrNone c1 stmtList

parseImport :: P.Parser (Maybe RawImport, C)
parseImport = do
  choice
    [ do
        c1 <- P.keyword "import"
        m <- P.getCurrentHint
        (importItems, c) <- P.seriesBraceList $ do
          locator <- P.symbol
          (lls, c) <- parseLocalLocatorList'
          return (RawImportItem m locator lls, c)
        return (Just $ RawImport c1 m importItems, c),
      return (Nothing, [])
    ]

parseStmt :: P.Parser (RawStmt, C)
parseStmt = do
  choice
    [ parseDefine,
      parseData,
      parseInline,
      parseConstant,
      parseNominal,
      parseResource,
      parseForeign
    ]

parseLocalLocatorList' :: P.Parser (SE.Series (Hint, LL.LocalLocator), C)
parseLocalLocatorList' = do
  choice
    [ P.seriesBrace parseLocalLocator,
      return (SE.emptySeries SE.Brace SE.Comma, [])
    ]

parseLocalLocator :: P.Parser ((Hint, LL.LocalLocator), C)
parseLocalLocator = do
  m <- P.getCurrentHint
  (ll, c) <- P.baseName
  return ((m, LL.new ll), c)

parseForeign :: P.Parser (RawStmt, C)
parseForeign = do
  c1 <- P.keyword "foreign"
  (val, c) <- P.seriesBraceList parseForeignItem
  return (RawStmtForeign c1 val, c)

parseForeignItem :: P.Parser (RawForeignItem, C)
parseForeignItem = do
  (funcName, c1) <- P.symbol
  m <- P.getCurrentHint
  (lts, c2) <- P.seriesParen lowType
  c3 <- P.delimiter ":"
  (cod, c) <- lowType
  return (RawForeignItem m (EN.ExternalName funcName) c1 lts c2 c3 cod, c)

parseDefine :: P.Parser (RawStmt, C)
parseDefine =
  parseDefine' O.Opaque

parseInline :: P.Parser (RawStmt, C)
parseInline =
  parseDefine' O.Clear

parseDefine' :: O.Opacity -> P.Parser (RawStmt, C)
parseDefine' opacity = do
  c1 <-
    case opacity of
      O.Opaque ->
        P.keyword "define"
      O.Clear ->
        P.keyword "inline"
  (def, c) <- parseDef return
  return (RawStmtDefine c1 (SK.Normal opacity) def, c)

parseData :: P.Parser (RawStmt, C)
parseData = do
  c1 <- P.keyword "data"
  m <- P.getCurrentHint
  (dataName, c2) <- P.baseName
  dataArgsOrNone <- parseDataArgs
  (consSeries, c) <- P.seriesBraceList parseDefineDataClause
  return (RawStmtDefineData c1 m (dataName, c2) dataArgsOrNone consSeries, c)

parseNominal :: P.Parser (RawStmt, C)
parseNominal = do
  c1 <- P.keyword "nominal"
  m <- P.getCurrentHint
  (geists, c) <- P.seriesBraceList $ parseGeist return
  return (RawStmtNominal c1 m geists, c)

parseDataArgs :: P.Parser (Maybe (RT.Args RT.RawTerm))
parseDataArgs = do
  choice
    [ Just <$> try (P.seqOrList preBinder),
      return Nothing
    ]

parseDefineDataClause :: P.Parser (RawConsInfo BN.BaseName, C)
parseDefineDataClause = do
  m <- P.getCurrentHint
  consName@(consName', _) <- P.baseName
  unless (isConsName (BN.reify consName')) $ do
    lift $ Throw.raiseError m "the name of a constructor must be capitalized"
  (consArgsOrNone, c) <- parseConsArgs
  let consArgs = fromMaybe SE.emptySeriesPC consArgsOrNone
  let isConstLike = isNothing consArgsOrNone
  return ((m, consName, isConstLike, consArgs), c)

parseConsArgs :: P.Parser (Maybe (SE.Series (RawBinder RT.RawTerm)), C)
parseConsArgs = do
  choice
    [ do
        (series, c) <- P.seqOrList parseDefineDataClauseArg
        return (Just series, c),
      return (Nothing, [])
    ]

parseDefineDataClauseArg :: P.Parser (RawBinder RT.RawTerm, C)
parseDefineDataClauseArg = do
  choice
    [ try preAscription,
      typeWithoutIdent
    ]

parseResource :: P.Parser (RawStmt, C)
parseResource = do
  c1 <- P.keyword "resource"
  m <- P.getCurrentHint
  (name, c2) <- P.baseName
  (c3, ((discarder, copier), c4)) <- P.betweenBrace $ do
    cDiscarder <- P.delimiter "-"
    discarder <- rawExpr
    cCopier <- P.delimiter "-"
    copier <- rawExpr
    return ((cDiscarder, discarder), (cCopier, copier))
  return (RawStmtDefineResource c1 m (name, c2) c3 discarder copier, c4)

parseConstant :: P.Parser (RawStmt, C)
parseConstant = do
  c1 <- P.keyword "constant"
  m <- P.getCurrentHint
  (constName, c2) <- P.baseName
  t <- parseDefInfoCod m
  (c3, (v, c4)) <- P.betweenBrace rawExpr
  return (RawStmtDefineConst c1 m (constName, c2) t (c3, v), c4)
