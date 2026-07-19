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
import Language.Common.DataInfo (FieldHint (..))
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
  stmtList <- many $ parseStmt False h
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
    locator <- locatorSymbol
    case fst locator of
      "static-file" -> do
        (ks, c) <- parseStaticKeyList
        return (RawStaticFileKey m c1 ks, c)
      _ -> do
        (entries, c) <- parseImportEntryList
        return (RawImportItem mImportItem locator entries, c)
  return (RawImport c1 m importItems loc, c)

parseStmt :: Bool -> Handle -> Parser (RawStmt, C)
parseStmt isInNamespace h = do
  choice
    [ parseDefine isInNamespace h,
      parseData h,
      parseInline isInNamespace h,
      parseConstantMeta isInNamespace h,
      parseConstant isInNamespace h,
      parseMacro isInNamespace h,
      parseMacroInline isInNamespace h,
      parseAlias h,
      parseAliasOpaque h,
      parseTrope h,
      parseNominal h,
      parseNamespace h,
      parseResource h,
      parseVariadic h FoldLeft,
      parseVariadic h FoldRight,
      parseForeign h
    ]

parseNamespace :: Handle -> Parser (RawStmt, C)
parseNamespace h = do
  c1 <- keyword "namespace"
  m <- getCurrentHint
  (name, c2) <- baseName
  c3 <- delimiter "{"
  stmtList <- many $ parseStmt True h
  loc <- getCurrentLoc
  c <- delimiter "}"
  return (RawStmtNamespace c1 m (name, c2) c3 stmtList loc, c)

parseImportEntryList :: Parser (SE.Series RawImportEntry, C)
parseImportEntryList = do
  choice
    [ seriesBrace parseImportEntry,
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

parseImportEntry :: Parser (RawImportEntry, C)
parseImportEntry = do
  m <- getCurrentHint
  choice
    [ do
        c1 <- delimiter "*"
        (asClause, c) <- parseAsClause c1
        return (RawImportWildcard m asClause, c),
      do
        (name, c1) <- baseName
        asClauseOrNone <- optional $ parseAsClause c1
        case asClauseOrNone of
          Just (asClause, c) ->
            return (RawImportName m (LL.new name) (Just asClause), c)
          Nothing ->
            return (RawImportName m (LL.new name) Nothing, c1)
    ]

parseAsClause :: C -> Parser (RawAsClause, C)
parseAsClause precedingComment = do
  aliasKeywordComment <- keyword "as"
  m <- getCurrentHint
  (importAlias, trailingComment) <- baseName
  return (RawAsClause precedingComment aliasKeywordComment m importAlias, trailingComment)

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
  c3 <- delimiter "->"
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

checkNotMainOrZen :: Bool -> BN.BaseName -> Hint -> T.Text -> Parser ()
checkNotMainOrZen isInNamespace defName m keywordName = do
  unless isInNamespace $ do
    when (defName == BN.mainName) $ do
      lift $ raiseError m $ "`main` must be defined using `define`, not `" <> keywordName <> "`"
    when (defName == BN.zenName) $ do
      lift $ raiseError m $ "`zen` must be defined using `define`, not `" <> keywordName <> "`"

parseDefine :: Bool -> Handle -> Parser (RawStmt, C)
parseDefine isInNamespace h = do
  c1 <- keyword "define"
  (def, c) <- parseDef ArrowObject h baseName
  let defName = RT.getDefName def
  let isDestPassing = RT.isDestPassing $ RT.geist def
  if not isInNamespace && (defName == BN.mainName || defName == BN.zenName)
    then do
      let m = RT.loc $ RT.geist def
      if isDestPassing
        then lift $ raiseError m "`main` and `zen` cannot use `->>`"
        else return (RawStmtDefineTerm c1 (SK.Main ()) def, c)
    else
      if isDestPassing
        then return (RawStmtDefineTerm c1 SK.DestPassing def, c)
        else return (RawStmtDefineTerm c1 SK.Define def, c)

parseMacro :: Bool -> Handle -> Parser (RawStmt, C)
parseMacro isInNamespace h = do
  c1 <- keyword "define-meta"
  (def, c) <- parseDef ArrowMeta h baseName
  let defName = RT.getDefName def
  let m = RT.loc $ RT.geist def
  checkNotMainOrZen isInNamespace defName m "define-meta"
  return (RawStmtDefineTerm c1 SK.Macro def, c)

parseMacroInline :: Bool -> Handle -> Parser (RawStmt, C)
parseMacroInline isInNamespace h = do
  c1 <- keyword "inline-meta"
  (def, c) <- parseDef ArrowMeta h baseName
  let defName = RT.getDefName def
  let m = RT.loc $ RT.geist def
  checkNotMainOrZen isInNamespace defName m "inline-meta"
  return (RawStmtDefineTerm c1 SK.MacroInline def, c)

parseInline :: Bool -> Handle -> Parser (RawStmt, C)
parseInline isInNamespace h = do
  c1 <- keyword "inline"
  (def, c) <- parseDef ArrowObject h baseName
  let defName = RT.getDefName def
  let m = RT.loc $ RT.geist def
  checkNotMainOrZen isInNamespace defName m "inline"
  if RT.isDestPassing $ RT.geist def
    then return (RawStmtDefineTerm c1 SK.DestPassingInline def, c)
    else return (RawStmtDefineTerm c1 SK.Inline def, c)

parseConstant :: Bool -> Handle -> Parser (RawStmt, C)
parseConstant isInNamespace h = do
  c1 <- keyword "constant"
  (def, c) <- parseConstantDef h baseName
  let defName = RT.getDefName def
  let m = RT.loc $ RT.geist def
  checkNotMainOrZen isInNamespace defName m "constant"
  return (RawStmtDefineTerm c1 SK.Constant def, c)

parseConstantMeta :: Bool -> Handle -> Parser (RawStmt, C)
parseConstantMeta isInNamespace h = do
  c1 <- keyword "constant-meta"
  (def, c) <- parseConstantDef h baseName
  let defName = RT.getDefName def
  let m = RT.loc $ RT.geist def
  checkNotMainOrZen isInNamespace defName m "constant-meta"
  return (RawStmtDefineTerm c1 SK.ConstantMeta def, c)

parseAlias :: Handle -> Parser (RawStmt, C)
parseAlias h = do
  c1 <- keyword "alias"
  (def, c) <- parseAliasDef h baseName
  return (RawStmtDefineType c1 TransparentAlias def, c)

parseAliasOpaque :: Handle -> Parser (RawStmt, C)
parseAliasOpaque h = do
  c1 <- keyword "alias-opaque"
  (def, c) <- parseAliasDef h baseName
  return (RawStmtDefineType c1 OpaqueAlias def, c)

parseData :: Handle -> Parser (RawStmt, C)
parseData h = do
  c1 <- keyword "data"
  m <- getCurrentHint
  (dataName, c2) <- baseName
  dataArgsOrNone <- parseDataArgs h
  (consSeries, loc, c) <- seriesBraceList' $ parseDefineDataClause h
  return (RawStmtDefineData c1 m (dataName, c2) dataArgsOrNone consSeries loc, c)

parseTrope :: Handle -> Parser (RawStmt, C)
parseTrope h = do
  c1 <- keyword "trope"
  m <- getCurrentHint
  name <- baseName
  (defineMetaList, loc, c) <- parseDefineMetaBlock h
  return (RawStmtTrope c1 m name defineMetaList loc, c)

parseDefineMetaBlock :: Handle -> Parser (SE.Series (RawDefineMeta Name), Loc, C)
parseDefineMetaBlock h = do
  c1 <- delimiter "{"
  entries <- many $ parseDefineMeta h
  loc <- getCurrentLoc
  c2 <- delimiter "}"
  let defineMetaSeries =
        case entries of
          [] ->
            (SE.emptySeries (Just SE.Brace) SE.Comma)
              { SE.trailingComment = c1
              }
          entry : rest ->
            SE.fromListWithComment (Just SE.Brace) SE.Comma $ (c1, entry) : map ([],) rest
  return (defineMetaSeries, loc, c2)

parseDefineMeta :: Handle -> Parser (RawDefineMeta Name, C)
parseDefineMeta h = do
  c1 <- keyword "define-meta"
  m <- getCurrentHint
  mTarget <- getCurrentHint
  (targetText, cTarget) <- symbol
  target <- interpretName mTarget targetText
  targetArgs <- seriesAngle $ rawType h
  expArgs <- seriesParen $ mandatoryBinder h
  (_, cArrow, cod) <- parseDefInfoCod ArrowMeta h
  (c2, ((body, _), loc, c)) <- betweenBrace' $ rawExpr h
  return
    ( RawDefineMeta
        { defineMetaLoc = m,
          defineMetaTarget = (target, cTarget),
          defineMetaTargetArgs = targetArgs,
          defineMetaExpArgs = expArgs,
          defineMetaCod = (cArrow, fst cod),
          defineMetaBody = body,
          defineMetaEndLoc = loc
        },
      c1 ++ c2 ++ c
    )

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
        (geist, cGeist) <- parseNominalGeist ArrowObject h baseName
        loc <- getCurrentLoc
        let kind = if RT.isDestPassing geist then DestPassing else Define
        return ((kind, geist, loc), cTag ++ cGeist),
      do
        cTag <- keyword "inline"
        (geist, cGeist) <- parseNominalGeist ArrowObject h baseName
        loc <- getCurrentLoc
        let kind = if RT.isDestPassing geist then DestPassingInline else Inline
        return ((kind, geist, loc), cTag ++ cGeist),
      do
        cTag <- keyword "constant-meta"
        (geist, cGeist) <- parseConstantGeist h baseName
        loc <- getCurrentLoc
        return ((ConstantMeta, geist, loc), cTag ++ cGeist),
      do
        cTag <- keyword "constant"
        (geist, cGeist) <- parseConstantGeist h baseName
        loc <- getCurrentLoc
        return ((Constant, geist, loc), cTag ++ cGeist),
      do
        cTag <- keyword "define-meta"
        (geist, cGeist) <- parseNominalGeist ArrowMeta h baseName
        loc <- getCurrentLoc
        return ((Macro, geist, loc), cTag ++ cGeist),
      do
        cTag <- keyword "inline-meta"
        (geist, cGeist) <- parseNominalGeist ArrowMeta h baseName
        loc <- getCurrentLoc
        return ((MacroInline, geist, loc), cTag ++ cGeist),
      do
        cTag <- keyword "alias-opaque"
        (geist, cGeist) <- parseAliasGeist h baseName
        loc <- getCurrentLoc
        return ((AliasOpaque, geist, loc), cTag ++ cGeist),
      do
        cTag <- keyword "alias"
        (geist, cGeist) <- parseAliasGeist h baseName
        loc <- getCurrentLoc
        return ((Alias, geist, loc), cTag ++ cGeist),
      do
        cTag <- keyword "data"
        (geist, cGeist) <- parseNominalData h
        loc <- getCurrentLoc
        return ((Data, geist, loc), cTag ++ cGeist),
      do
        cTag <- keyword "resource"
        (geist, cGeist) <- parseResourceGeist baseName
        loc <- getCurrentLoc
        return ((Resource, geist, loc), cTag ++ cGeist)
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
            isDestPassing = False,
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

parseConsArgs :: Handle -> Parser (Maybe (SE.Series (FieldHint, RawBinder RT.RawType)), Loc, C)
parseConsArgs h = do
  choice
    [ do
        (s, loc, c) <- seriesParen' $ parseDefineDataClauseArg h
        return (Just s, loc, c),
      do
        loc <- getCurrentLoc
        return (Nothing, loc, [])
    ]

parseDefineDataClauseArg :: Handle -> Parser ((FieldHint, RawBinder RT.RawType), C)
parseDefineDataClauseArg h = do
  (binder, c) <- parseDataClauseArgBinder h
  mMixed <- optional $ do
    mMix <- getCurrentHint
    cMix <- keyword "mix"
    return (mMix, cMix)
  case mMixed of
    Just (mMix, cMix) ->
      return ((FieldMixed mMix, binder), c ++ cMix)
    Nothing ->
      return ((FieldAuto, binder), c)

parseDataClauseArgBinder :: Handle -> Parser (RawBinder RT.RawType, C)
parseDataClauseArgBinder h = do
  choice
    [ try $ varWithMode h >>= preAscription h,
      typeWithoutIdent h
    ]

parseResource :: Handle -> Parser (RawStmt, C)
parseResource h = do
  c1 <- keyword "resource"
  m <- getCurrentHint
  (name, c2) <- baseName
  (handlers, c) <- seriesBrace $ rawExpr h
  case SE.elems handlers of
    [discarder, copier, resourceSize] -> do
      return (RawStmtDefineResource c1 m (name, c2) discarder copier resourceSize (SE.trailingComment handlers), c)
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
