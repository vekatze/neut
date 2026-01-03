module Kernel.Parse.Internal.Program
  ( parseProgram,
    parseImport,
  )
where

import App.Run (raiseError)
import CodeParser.GetInfo
import CodeParser.Parser
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Trans
import Data.Text qualified as T
import Kernel.Parse.Internal.RawTerm
import Language.Common.BaseName qualified as BN
import Language.Common.ExternalName qualified as EN
import Language.Common.ForeignCodType qualified as F
import Language.Common.LocalLocator qualified as LL
import Language.Common.NominalTag
import Language.Common.RuleKind
import Language.Common.StmtKind qualified as SK
import Language.RawTerm.CreateHole qualified as RT
import Language.RawTerm.Name
import Language.RawTerm.RawBinder
import Language.RawTerm.RawStmt
import Language.RawTerm.RawTerm qualified as RT
import Logger.Hint
import SyntaxTree.C
import SyntaxTree.ParseSeries
import SyntaxTree.Series qualified as SE
import Text.Megaparsec

parseProgram :: Handle -> Parser RawProgram
parseProgram h = do
  m <- getCurrentHint
  importList <- parseImport
  stmtList <- many $ parseStmt h
  return $ RawProgram m importList stmtList

parseImport :: Parser [(RawImport, C)]
parseImport =
  many parseSingleImport

parseSingleImport :: Parser (RawImport, C)
parseSingleImport = do
  c1 <- keyword "import"
  m <- getCurrentHint
  (importItems, loc, c) <- seriesBrace' $ do
    mImportItem <- getCurrentHint
    locator <- symbol
    case fst locator of
      "static" -> do
        (ks, c) <- parseStaticKeyList
        return (RawStaticKey m c1 ks, c)
      _ -> do
        (lls, c) <- parseLocalLocatorList'
        return (RawImportItem mImportItem locator lls, c)
  return (RawImport c1 m importItems loc, c)

parseStmt :: Handle -> Parser (RawStmt, C)
parseStmt h = do
  choice
    [ parseDefine h,
      parseData h,
      parseInline h,
      parseMacro h,
      parseAlias h,
      parseNominal h,
      parseResource h,
      parseVariadic h FoldLeft,
      parseVariadic h FoldRight,
      parseForeign h
    ]

parseLocalLocatorList' :: Parser (SE.Series (Hint, LL.LocalLocator), C)
parseLocalLocatorList' = do
  choice
    [ seriesBrace parseLocalLocator,
      return (SE.emptySeries (Just SE.Brace) SE.Comma, [])
    ]

parseStaticKeyList :: Parser (SE.Series (Hint, T.Text), C)
parseStaticKeyList = do
  choice
    [ seriesBrace $ do
        m <- getCurrentHint
        (k, c) <- symbol
        return ((m, k), c),
      return (SE.emptySeries (Just SE.Brace) SE.Comma, [])
    ]

parseLocalLocator :: Parser ((Hint, LL.LocalLocator), C)
parseLocalLocator = do
  m <- getCurrentHint
  (ll, c) <- baseName
  return ((m, LL.new ll), c)

parseForeign :: Handle -> Parser (RawStmt, C)
parseForeign h = do
  c1 <- keyword "foreign"
  (val, c) <- seriesBrace $ parseForeignItem h
  return (RawStmtForeign c1 val, c)

parseForeignItem :: Handle -> Parser (RawForeignItem, C)
parseForeignItem h = do
  m <- getCurrentHint
  (funcName, c1) <- symbol
  (domList, c2) <- seriesParen $ rawType h
  c3 <- delimiter ":"
  (cod, c) <-
    choice
      [ do
          c <- keyword "void"
          return (F.Void, c),
        do
          (lt, c) <- rawType h
          return (F.Cod lt, c)
      ]
  return (RawForeignItemF m (EN.ExternalName funcName) c1 domList c2 c3 cod, c)

checkNotMainOrZen :: BN.BaseName -> Hint -> T.Text -> Parser ()
checkNotMainOrZen defName m keywordName = do
  when (defName == BN.mainName) $ do
    lift $ raiseError m $ "`main` must be defined using `define`, not `" <> keywordName <> "`"
  when (defName == BN.zenName) $ do
    lift $ raiseError m $ "`zen` must be defined using `define`, not `" <> keywordName <> "`"

parseDefine :: Handle -> Parser (RawStmt, C)
parseDefine h = do
  c1 <- keyword "define"
  (def, c) <- parseDef h baseName
  let defName = RT.getDefName def
  if defName == BN.mainName || defName == BN.zenName
    then return (RawStmtDefineTerm c1 (SK.Main ()) def, c)
    else return (RawStmtDefineTerm c1 SK.Define def, c)

parseMacro :: Handle -> Parser (RawStmt, C)
parseMacro h = do
  c1 <- keyword "macro"
  (def, c) <- parseDef h baseName
  let defName = RT.getDefName def
  let m = RT.loc $ RT.geist def
  checkNotMainOrZen defName m "macro"
  return (RawStmtDefineTerm c1 SK.Macro def, c)

parseInline :: Handle -> Parser (RawStmt, C)
parseInline h = do
  c1 <- keyword "inline"
  (def, c) <- parseDef h baseName
  let defName = RT.getDefName def
  let m = RT.loc $ RT.geist def
  checkNotMainOrZen defName m "inline"
  return (RawStmtDefineTerm c1 SK.Inline def, c)

parseAlias :: Handle -> Parser (RawStmt, C)
parseAlias h = do
  c1 <- keyword "alias"
  (def, c) <- parseAliasDef h baseName
  return (RawStmtDefineType c1 def, c)

parseData :: Handle -> Parser (RawStmt, C)
parseData h = do
  c1 <- keyword "data"
  m <- getCurrentHint
  (dataName, c2) <- baseName
  dataArgsOrNone <- parseDataArgs h
  (consSeries, loc, c) <- seriesBraceList' $ parseDefineDataClause h
  return (RawStmtDefineData c1 m (dataName, c2) dataArgsOrNone consSeries loc, c)

parseNominal :: Handle -> Parser (RawStmt, C)
parseNominal h = do
  c1 <- keyword "nominal"
  m <- getCurrentHint
  (geists, c) <- seriesBrace $ parseNominalEntry h
  return (RawStmtNominal c1 m geists, c)

parseNominalEntry :: Handle -> Parser ((NominalTag, RT.RawGeist BN.BaseName, Loc), C)
parseNominalEntry h =
  choice
    [ do
        cTag <- keyword "define"
        (geist, cGeist) <- parseGeist h baseName
        loc <- getCurrentLoc
        return ((Define, geist, loc), cTag ++ cGeist),
      do
        cTag <- keyword "inline"
        (geist, cGeist) <- parseGeist h baseName
        loc <- getCurrentLoc
        return ((Inline, geist, loc), cTag ++ cGeist),
      do
        cTag <- keyword "alias"
        (geist, cGeist) <- parseAliasGeist h baseName
        loc <- getCurrentLoc
        return ((Alias, geist, loc), cTag ++ cGeist),
      do
        cTag <- keyword "data"
        (geist, cGeist) <- parseNominalData h
        loc <- getCurrentLoc
        return ((Data, geist, loc), cTag ++ cGeist)
    ]

parseNominalData :: Handle -> Parser (RT.RawGeist BN.BaseName, C)
parseNominalData h = do
  m <- getCurrentHint
  (name, c1) <- baseName
  argsOrNone <- parseDataArgs h
  let expArgs = maybe RT.emptyArgs id argsOrNone
  let isConstLike = maybe True (const False) argsOrNone
  let geist =
        RT.RawGeist
          { loc = m,
            name = (name, c1),
            isConstLike = isConstLike,
            impArgs = RT.emptyImpArgs,
            defaultArgs = RT.emptyDefaultArgs,
            expArgs = expArgs,
            cod = ([], m :< RT.Tau)
          }
  return (geist, [])

parseDataArgs :: Handle -> Parser (Maybe (RT.Args RT.RawType))
parseDataArgs h = do
  choice
    [ Just <$> try (seriesParen $ preBinder h),
      return Nothing
    ]

parseDefineDataClause :: Handle -> Parser (RawConsInfo BN.BaseName, C)
parseDefineDataClause h = do
  loc <- getCurrentHint
  (name, c1) <- baseName
  unless (isConsName (BN.reify name)) $ do
    lift $ raiseError loc "The name of a constructor must be capitalized"
  (expArgs, endLoc, c2) <- parseConsArgs h
  return (RawConsInfo {loc, name, expArgs, endLoc}, c1 ++ c2)

parseConsArgs :: Handle -> Parser (Maybe (SE.Series (RawBinder RT.RawType)), Loc, C)
parseConsArgs h = do
  choice
    [ do
        (s, loc, c) <- seriesParen' $ parseDefineDataClauseArg h
        return (Just s, loc, c),
      do
        loc <- getCurrentLoc
        return (Nothing, loc, [])
    ]

parseDefineDataClauseArg :: Handle -> Parser (RawBinder RT.RawType, C)
parseDefineDataClauseArg h = do
  choice
    [ try $ var h >>= preAscription h,
      typeWithoutIdent h
    ]

parseResource :: Handle -> Parser (RawStmt, C)
parseResource h = do
  c1 <- keyword "resource"
  m <- getCurrentHint
  (name, c2) <- baseName
  (handlers, c) <- seriesBrace $ rawExpr h
  case SE.elems handlers of
    [discarder, copier, typeTag] -> do
      return (RawStmtDefineResource c1 m (name, c2) discarder copier typeTag (SE.trailingComment handlers), c)
    _ ->
      lift $ raiseError m $ "`resource` must have 3 elements, but found: " <> T.pack (show $ length $ SE.elems handlers)

parseVariadic :: Handle -> RuleKind -> Parser (RawStmt, C)
parseVariadic h vk = do
  let k = ruleKindToKeyword vk
  c1 <- keyword k
  m <- getCurrentHint
  (name, c2) <- baseName
  (handlers, loc, c) <- seriesBrace' $ rawExpr h
  case SE.elems handlers of
    [(cLeaf, leaf), (cNode, node), (cRoot, root)] -> do
      nodeType <- liftIO $ RT.createTypeHole (gensymHandle h) m
      leafType <- liftIO $ RT.createTypeHole (gensymHandle h) m
      rootType <- liftIO $ RT.createTypeHole (gensymHandle h) m
      let l = (cLeaf, leaf, leafType)
      let n = (cNode, node, nodeType)
      let r = (cRoot, root, rootType)
      return (RawStmtVariadic vk c1 m (name, c2) l n r (SE.trailingComment handlers) loc, c)
    _ -> do
      lift $ raiseError m $ "`" <> k <> "` must have 3 elements, but found: " <> T.pack (show $ length $ SE.elems handlers)
