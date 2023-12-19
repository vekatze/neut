module Scene.Parse.Program
  ( parseProgram,
    parseImport,
  )
where

import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Entity.BaseName qualified as BN
import Entity.C
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.IsConstLike
import Entity.LocalLocator qualified as LL
import Entity.Name
import Entity.Opacity qualified as O
import Entity.RawBinder
import Entity.RawDecl qualified as RD
import Entity.RawProgram
import Entity.RawTerm qualified as RT
import Entity.StmtKind qualified as SK
import Scene.Parse.Core qualified as P
import Scene.Parse.RawTerm
import Text.Megaparsec

parseProgram :: P.Parser RawProgram
parseProgram = do
  m <- P.getCurrentHint
  (importBlockOrNone, c1) <- parseImport
  (foreignOrNone, c2) <- parseForeign
  stmtList <- many parseStmt
  return $ RawProgram m importBlockOrNone c1 foreignOrNone c2 stmtList

parseStmt :: P.Parser (RawStmt, C)
parseStmt = do
  choice
    [ parseDefine,
      parseData,
      parseInline,
      parseConstant,
      parseDeclare,
      parseResource
    ]

parseImport :: P.Parser (Maybe RawImport, C)
parseImport = do
  choice
    [ do
        c1 <- P.keyword "import"
        m <- P.getCurrentHint
        (c2, (items, c)) <- P.betweenBrace $ P.manyList $ do
          locator <- P.symbol
          RawImportItem c1 m locator <$> parseLocalLocatorList'
        return (Just $ RawImport c1 m (c2, items), c),
      return (Nothing, [])
    ]

parseLocalLocatorList' :: P.Parser ([(C, ((Hint, LL.LocalLocator), C))], C)
parseLocalLocatorList' = do
  choice
    [ P.argListBrace parseLocalLocator,
      return ([], [])
    ]

parseLocalLocator :: P.Parser ((Hint, LL.LocalLocator), C)
parseLocalLocator = do
  m <- P.getCurrentHint
  (ll, c) <- P.baseName
  return ((m, LL.new ll), c)

parseForeign :: P.Parser (Maybe RawForeign, C)
parseForeign = do
  choice
    [ do
        c1 <- P.keyword "foreign"
        (c2, (val, c)) <- P.betweenBrace (P.manyList parseForeignItem)
        return (Just $ RawForeign c1 (c2, val), c),
      return (Nothing, [])
    ]

parseForeignItem :: P.Parser RawForeignItem
parseForeignItem = do
  (declName, c1) <- P.symbol
  lts <- P.argListParen lowType
  c2 <- P.delimiter ":"
  (cod, c) <- lowType
  return $ RawForeignItem (EN.ExternalName declName) c1 lts c2 (cod, c)

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
  m <- P.getCurrentHint
  (((_, (name, c)), impArgs, expArgs, codType), (c2, (e, cCont))) <- parseTopDefInfo
  name' <- lift $ Locator.attachCurrentLocator name
  let stmtKind = SK.Normal opacity
  let decl =
        RD.RawDecl
          { loc = m,
            name = (name', c),
            isConstLike = False,
            impArgs = impArgs,
            expArgs = expArgs,
            cod = codType
          }
  return (RawStmtDefine c1 stmtKind decl (c2, e), cCont)

parseData :: P.Parser (RawStmt, C)
parseData = do
  c1 <- P.keyword "data"
  m <- P.getCurrentHint
  (dataName, c2) <- P.baseName
  dataName' <- lift $ Locator.attachCurrentLocator dataName
  dataArgsOrNone <- parseDataArgs
  (c3, (consInfoList, c)) <- P.betweenBrace $ P.manyList parseDefineDataClause
  return (RawStmtDefineData c1 m (dataName', c2) dataArgsOrNone c3 consInfoList, c)

parseDeclare :: P.Parser (RawStmt, C)
parseDeclare = do
  c1 <- P.keyword "declare"
  m <- P.getCurrentHint
  (c2, (decls, c3)) <- P.betweenBrace $ P.manyList $ parseDeclareItem Locator.attachCurrentLocator
  return (RawStmtDeclare c1 m c2 decls, c3)

parseDataArgs :: P.Parser (Maybe RD.ExpArgs)
parseDataArgs = do
  choice
    [ do
        args <- try (P.argSeqOrList preBinder)
        return $ Just args,
      return Nothing
    ]

parseDefineDataClause :: P.Parser (Hint, (BN.BaseName, C), IsConstLike, RD.ExpArgs)
parseDefineDataClause = do
  m <- P.getCurrentHint
  consName@(consName', _) <- P.baseName
  unless (isConsName (BN.reify consName')) $ do
    lift $ Throw.raiseError m "the name of a constructor must be capitalized"
  consArgsOrNone <- parseConsArgs
  let consArgs = fromMaybe (Nothing, ([], [])) consArgsOrNone
  let isConstLike = isNothing consArgsOrNone
  return (m, consName, isConstLike, consArgs)

parseConsArgs :: P.Parser (Maybe RD.ExpArgs)
parseConsArgs = do
  choice
    [ do
        args <- P.argSeqOrList parseDefineDataClauseArg
        return $ Just args,
      return Nothing
    ]

parseDefineDataClauseArg :: P.Parser (RawBinder (RT.RawTerm, C))
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
  name' <- lift $ Locator.attachCurrentLocator name
  (c3, ((discarder, copier), c4)) <- P.betweenBrace $ do
    cDiscarder <- P.delimiter "-"
    discarder <- rawExpr
    cCopier <- P.delimiter "-"
    copier <- rawExpr
    return ((cDiscarder, discarder), (cCopier, copier))
  return (RawStmtDefineResource c1 m (name', c2) c3 discarder copier, c4)

parseConstant :: P.Parser (RawStmt, C)
parseConstant = do
  c1 <- P.keyword "constant"
  m <- P.getCurrentHint
  (constName, c2) <- P.baseName
  constName' <- lift $ Locator.attachCurrentLocator constName
  t <- parseDefInfoCod m
  (c3, (v, c4)) <- P.betweenBrace rawExpr
  return (RawStmtDefineConst c1 m (constName', c2) t (c3, v), c4)
