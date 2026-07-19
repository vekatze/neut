module Language.RawTerm.RawStmt.ToDoc (pp, ImportInfo (..)) where

import Control.Monad
import Data.Bifunctor
import Data.Text qualified as T
import Language.Common.BaseName qualified as BN
import Language.Common.Const (doubleColon)
import Language.Common.ExternalName qualified as EN
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.GlobalLocator qualified as GL
import Language.Common.LocalLocator qualified as LL
import Language.Common.ModuleAlias qualified as MA
import Language.Common.NominalTag
import Language.Common.RuleKind (ruleKindToKeyword)
import Language.Common.StmtKind qualified as SK
import Language.Common.UnusedGlobalLocators (UnusedGlobalLocators, isUsedGL)
import Language.Common.UnusedLocalLocators (UnusedLocalLocators, isUsedLL)
import Language.RawTerm.Name qualified as N
import Language.RawTerm.RawStmt
import Language.RawTerm.RawTerm qualified as RT
import Language.RawTerm.RawTerm.ToDoc qualified as RT
import Logger.Hint
import PrettyPrinter.Doc qualified as D
import PrettyPrinter.Piece qualified as PI
import SyntaxTree.C
import SyntaxTree.C.ToDoc qualified as C
import SyntaxTree.Series (Series (hasOptionalSeparator))
import SyntaxTree.Series qualified as SE
import SyntaxTree.Series.ToDoc qualified as SE

data ImportInfo = ImportInfo
  { presetNames :: [(T.Text, [BN.BaseName])], -- "prelude"
    unusedLocalLocators :: UnusedLocalLocators,
    unusedGlobalLocators :: UnusedGlobalLocators
  }

pp :: ImportInfo -> (C, RawProgram) -> T.Text
pp importInfo (c1, RawProgram m importList stmtList) = do
  let (importList', c2) = mergeImportList m importList
  let importOrNone' = decImport importInfo importList'
  let stmtList' = map (first (Just . decStmt)) stmtList
  let program' = (importOrNone', c2) : stmtList'
  D.layout $ decTopDocList c1 program'

decTopDocList :: C -> [(Maybe D.Doc, C)] -> D.Doc
decTopDocList c docList =
  case docList of
    [] ->
      attachStmtComment c D.Nil
    [(Nothing, c')] ->
      attachStmtComment (c ++ c') D.Nil
    [(Just doc, c')] ->
      if null c'
        then attachStmtComment c $ D.join [doc, D.line]
        else attachStmtComment c $ D.join [doc, D.line, D.line, decTopDocList c' []]
    (Nothing, c') : rest ->
      decTopDocList (c ++ c') rest
    (Just doc, c') : rest -> do
      attachStmtComment c $ D.join [doc, D.line, D.line, decTopDocList c' rest]

decImport :: ImportInfo -> RawImport -> Maybe D.Doc
decImport importInfo importStmt = do
  if isImportEmpty importStmt
    then Nothing
    else do
      let (RawImport c _ importItemList _) = importStmt
      let importItemList' = normalizeImportItem <$> SE.compressEither (fmap (filterImport importInfo) importItemList)
      let importItemList'' = SE.assoc $ decImportItem <$> sortImport importItemList'
      if SE.isEmpty importItemList''
        then Nothing
        else do
          return $
            attachStmtComment c $
              D.join
                [ D.text "import ",
                  SE.decode $ SE.assoc $ decImportItem <$> sortImport importItemList'
                ]

filterImport :: ImportInfo -> RawImportItem -> Either C RawImportItem
filterImport importInfo = do
  filterUnused importInfo >=> filterPreset importInfo

filterUnused :: ImportInfo -> RawImportItem -> Either C RawImportItem
filterUnused importInfo rawImportItem = do
  case rawImportItem of
    RawStaticFileKey {} ->
      return rawImportItem
    RawImportItem m (loc, c) entries -> do
      if isUsedGL (unusedGlobalLocators importInfo) loc
        then do
          let entries' = SE.filter (entryIsUsed importInfo) entries
          return $ RawImportItem m (loc, c) entries'
        else Left c

entryIsUsed :: ImportInfo -> RawImportEntry -> Bool
entryIsUsed importInfo entry =
  case entry of
    RawImportName _ ll asClauseOrNone ->
      isUsedLL (unusedLocalLocators importInfo) $ LL.new $ importEntryAlias ll asClauseOrNone
    RawImportWildcard _ (RawAsClause _ _ _ importAlias) ->
      isUsedLL (unusedLocalLocators importInfo) $ LL.new importAlias

importEntryAlias :: LL.LocalLocator -> Maybe RawAsClause -> BN.BaseName
importEntryAlias ll asClauseOrNone =
  case asClauseOrNone of
    Nothing ->
      LL.baseName ll
    Just (RawAsClause _ _ _ importAlias) ->
      importAlias

filterPreset :: ImportInfo -> RawImportItem -> Either C RawImportItem
filterPreset importInfo item = do
  case item of
    RawStaticFileKey {} ->
      return item
    RawImportItem m (loc, c) entries -> do
      case lookup loc (presetNames importInfo) of
        Nothing ->
          return item
        Just names -> do
          let entries' = SE.catMaybes $ fmap (filterPresetEntry names) entries
          if SE.isEmpty entries'
            then Left c
            else return $ RawImportItem m (loc, c) entries'

filterPresetEntry :: [BN.BaseName] -> RawImportEntry -> Maybe RawImportEntry
filterPresetEntry names entry =
  case entry of
    RawImportName _ ll Nothing
      | LL.baseName ll `elem` names ->
          Nothing
    _ ->
      Just entry

sortImport :: SE.Series RawImportItem -> SE.Series RawImportItem
sortImport series = do
  let series' = SE.sortSeriesBy compareImport series
  nubImportEntries . sortImportEntries <$> series' {SE.elems = mergeAdjacentImport (SE.elems series')}

compareImport :: RawImportItem -> RawImportItem -> Ordering
compareImport item1 item2 =
  case (item1, item2) of
    (RawImportItem _ (loc1, _) _, RawImportItem _ (loc2, _) _) ->
      compare (importSortKey loc1) (importSortKey loc2)
    (RawImportItem {}, RawStaticFileKey {}) ->
      LT
    (RawStaticFileKey {}, RawImportItem {}) ->
      GT
    (RawStaticFileKey {}, RawStaticFileKey {}) ->
      EQ

importSortKey :: T.Text -> (Bool, T.Text)
importSortKey loc =
  ((MA.reify MA.thisModuleAlias <> doubleColon) `T.isPrefixOf` loc, loc)

normalizeImportItem :: RawImportItem -> RawImportItem
normalizeImportItem item =
  case item of
    RawImportItem m (loc, c) entries ->
      RawImportItem m (normalizeLocator m loc, c) entries
    RawStaticFileKey {} ->
      item

normalizeLocator :: Hint -> T.Text -> T.Text
normalizeLocator m loc =
  either (const loc) GL.reify (GL.reflect m loc)

mergeAdjacentImport :: [(C, RawImportItem)] -> [(C, RawImportItem)]
mergeAdjacentImport importList = do
  case importList of
    [] ->
      []
    [item] ->
      [item]
    (c1, item1) : (c2, item2) : rest -> do
      case (item1, item2) of
        (RawStaticFileKey m1 c1' ks1, RawStaticFileKey _ c2' ks2) -> do
          let item = RawStaticFileKey m1 (c1' ++ c2') (SE.appendLeftBiased ks1 ks2)
          mergeAdjacentImport $ (c1 ++ c2, item) : rest
        (RawImportItem m1 (locator1, c1') entries1, RawImportItem _ (locator2, c2') entries2)
          | locator1 == locator2 -> do
              let entries = SE.appendLeftBiased entries1 entries2
              let item = RawImportItem m1 (locator1, c1' ++ c2') entries
              mergeAdjacentImport $ (c1 ++ c2, item) : rest
        _ ->
          (c1, item1) : mergeAdjacentImport ((c2, item2) : rest)

importEntryKey :: RawImportEntry -> (Int, T.Text, T.Text)
importEntryKey entry =
  case entry of
    RawImportWildcard _ (RawAsClause _ _ _ importAlias) ->
      (0, BN.reify importAlias, "")
    RawImportName _ ll asClauseOrNone ->
      (1, LL.reify ll, maybe "" (\(RawAsClause _ _ _ importAlias) -> BN.reify importAlias) asClauseOrNone)

sortImportEntries :: RawImportItem -> RawImportItem
sortImportEntries rawImportItem = do
  case rawImportItem of
    RawImportItem m locator entries -> do
      let cmp x y = compare (importEntryKey x) (importEntryKey y)
      RawImportItem m locator $ SE.sortSeriesBy cmp entries
    RawStaticFileKey m c ks -> do
      let cmp (_, x) (_, y) = compare x y
      RawStaticFileKey m c $ SE.sortSeriesBy cmp ks

nubImportEntries :: RawImportItem -> RawImportItem
nubImportEntries rawImportItem = do
  case rawImportItem of
    RawImportItem m locator entries -> do
      let eq x y = importEntryKey x == importEntryKey y
      RawImportItem m locator $ SE.nubSeriesBy eq entries
    RawStaticFileKey m c ks -> do
      let eq (_, x) (_, y) = x == y
      RawStaticFileKey m c $ SE.nubSeriesBy eq ks

decImportItem :: RawImportItem -> (D.Doc, C)
decImportItem rawImportItem = do
  case rawImportItem of
    RawImportItem _ (item, c) entries -> do
      if SE.isEmpty entries
        then (D.text item, c)
        else do
          let entries' = SE.pushComment c entries
          let entries'' = SE.decode $ fmap decImportEntry entries'
          (D.join [D.text item, D.text " ", entries''], [])
    RawStaticFileKey _ c ks -> do
      if SE.isEmpty ks
        then (D.Nil, c)
        else do
          let ks' = D.text . snd <$> SE.pushComment c ks
          (D.join [D.text "static-file", D.text " ", SE.decode ks'], [])

decImportEntry :: RawImportEntry -> D.Doc
decImportEntry entry =
  case entry of
    RawImportName _ ll asClauseOrNone ->
      decEntryAsClause (LL.reify ll) asClauseOrNone
    RawImportWildcard _ asClause ->
      decEntryAsClause "*" (Just asClause)

decEntryAsClause :: T.Text -> Maybe RawAsClause -> D.Doc
decEntryAsClause original asClauseOrNone =
  case asClauseOrNone of
    Nothing ->
      D.text original
    Just (RawAsClause beforeAs afterAs _ importAlias) -> do
      let beforeAsDoc =
            if null beforeAs
              then D.text " as"
              else D.join [C.asSuffix beforeAs, D.line, D.text "as"]
      let afterAsDoc =
            if null afterAs
              then D.text " "
              else D.join [C.asSuffix afterAs, D.line]
      D.join [D.text original, beforeAsDoc, afterAsDoc, D.text (BN.reify importAlias)]

decStmt :: RawStmt -> D.Doc
decStmt stmt =
  case stmt of
    RawStmtDefineTerm c stmtKind def -> do
      case stmtKind of
        SK.Define ->
          RT.decodeDef (RT.nameToDoc . N.Bare) "define" c (fmap BN.reify def)
        SK.DestPassing ->
          RT.decodeDef (RT.nameToDoc . N.Bare) "define" c (fmap BN.reify def)
        SK.DestPassingInline ->
          RT.decodeDef (RT.nameToDoc . N.Bare) "inline" c (fmap BN.reify def)
        SK.Inline ->
          if RT.isConstLike (RT.geist def)
            then RT.decodeDef (RT.nameToDoc . N.Bare) "constant" c (fmap BN.reify def)
            else RT.decodeDef (RT.nameToDoc . N.Bare) "inline" c (fmap BN.reify def)
        SK.Constant ->
          RT.decodeDef (RT.nameToDoc . N.Bare) "constant" c (fmap BN.reify def)
        SK.ConstantMeta ->
          RT.decodeDef (RT.nameToDoc . N.Bare) "constant-meta" c (fmap BN.reify def)
        SK.Macro ->
          RT.decodeDef (RT.nameToDoc . N.Bare) "define-meta" c (fmap BN.reify def)
        SK.MacroInline ->
          RT.decodeDef (RT.nameToDoc . N.Bare) "inline-meta" c (fmap BN.reify def)
        SK.Main _ ->
          RT.decodeDef (RT.nameToDoc . N.Bare) "define" c (fmap BN.reify def)
        _ ->
          RT.decodeDef (RT.nameToDoc . N.Bare) "define" c (fmap BN.reify def)
    RawStmtDefineType c aliasKind def -> do
      let keyword = case aliasKind of
            TransparentAlias -> "alias"
            OpaqueAlias -> "alias-opaque"
      RT.decodeTypeDef (RT.nameToDoc . N.Bare) keyword c (fmap BN.reify def)
    RawStmtDefineData c1 _ (dataName, c2) argsOrNone consInfo _ -> do
      attachStmtComment (c1 ++ c2) $
        D.join
          [ D.text "data ",
            D.text (BN.reify dataName),
            decDataArgs argsOrNone,
            D.text " ",
            SE.decode $ fmap decConsInfo consInfo
          ]
    RawStmtDefineResource c1 _ (name, c2) discarder copier resourceSize trailingComment -> do
      let series =
            SE.Series
              { elems = [discarder, copier, resourceSize],
                trailingComment,
                prefix = Nothing,
                container = Just SE.Brace,
                separator = SE.Comma,
                hasOptionalSeparator = True
              }
      attachStmtComment (c1 ++ c2) $
        PI.arrange
          [ PI.horizontal $ D.text "resource",
            PI.horizontal $ D.text $ BN.reify name,
            PI.inject $ SE.decode $ fmap RT.toDoc series
          ]
    RawStmtTrope c1 _ (name, c2) defineMetaList _ -> do
      attachStmtComment (c1 ++ c2) $
        PI.arrange
          [ PI.horizontal $ D.text "trope",
            PI.horizontal $ D.text $ BN.reify name,
            PI.inject $ decDefineMetaBlock defineMetaList
          ]
    RawStmtVariadic kind c1 _ (name, c2) (ct, leaf, _) (cn, node, _) (cr, root, _) trailingComment _ -> do
      let k = ruleKindToKeyword kind
      let series =
            SE.Series
              { elems = [(ct, leaf), (cn, node), (cr, root)],
                trailingComment,
                prefix = Nothing,
                container = Just SE.Brace,
                separator = SE.Comma,
                hasOptionalSeparator = True
              }
      attachStmtComment (c1 ++ c2) $
        PI.arrange
          [ PI.horizontal $ D.text k,
            PI.horizontal $ D.text (BN.reify name),
            PI.inject $ SE.decode $ fmap RT.toDoc series
          ]
    RawStmtNominal c _ geistList -> do
      attachStmtComment c $
        D.join
          [ D.text "nominal ",
            SE.decode $ fmap decNominalGeist geistList
          ]
    RawStmtForeign c foreignList -> do
      let foreignList' = SE.decode $ fmap decForeignItem foreignList
      attachStmtComment c $
        D.join
          [ D.text "foreign ",
            foreignList'
          ]
    RawStmtNamespace c1 _ (name, c2) c3 stmtList _ -> do
      attachStmtComment (c1 ++ c2) $
        D.join
          [ D.text "namespace ",
            D.text (BN.reify name),
            D.text " {",
            D.nest D.indent $ D.join [D.line, decNamespaceBody c3 stmtList],
            D.line,
            D.text "}"
          ]

decNamespaceBody :: C -> [(RawStmt, C)] -> D.Doc
decNamespaceBody c stmtList =
  case stmtList of
    [] ->
      attachStmtComment c D.Nil
    [(stmt, c')] ->
      attachStmtComment c $ D.join [decStmt stmt, C.asSuffix c']
    (stmt, c') : rest ->
      attachStmtComment c $ D.join [decStmt stmt, D.line, D.line, decNamespaceBody c' rest]

decForeignItem :: RawForeignItem -> D.Doc
decForeignItem (RawForeignItemF _ funcName _ args _ _ cod) = do
  let args' = SE.decode $ fmap RT.typeToDoc args
  let cod' =
        case cod of
          FCT.Cod c ->
            RT.typeToDoc c
          FCT.Void ->
            D.text "void"
  D.join [D.text (EN.reify funcName), args', D.text " -> ", cod']

decDefineMeta :: C -> RawDefineMeta N.Name -> D.Doc
decDefineMeta c defineMeta =
  RT.decodeDef (decDefineMetaTarget defineMeta) "define-meta" c $ defineMetaToDef defineMeta

decDefineMetaTarget :: RawDefineMeta N.Name -> N.Name -> D.Doc
decDefineMetaTarget defineMeta name = do
  let (targetArgs, c) = defineMetaTargetArgs defineMeta
  PI.arrange
    [ PI.inject $ RT.nameToDoc name,
      PI.inject $ RT.attachComment c $ SE.decodeHorizontallyIfPossible $ fmap RT.typeToDoc targetArgs
    ]

defineMetaToDef :: RawDefineMeta N.Name -> RT.RawDef N.Name
defineMetaToDef defineMeta = do
  let geist =
        RT.RawGeist
          { RT.loc = defineMetaLoc defineMeta,
            RT.name = defineMetaTarget defineMeta,
            RT.isConstLike = False,
            RT.isDestPassing = False,
            RT.impArgs = (SE.emptySeriesAC, []),
            RT.expArgs = defineMetaExpArgs defineMeta,
            RT.defaultArgs = RT.emptyDefaultArgs,
            RT.cod = defineMetaCod defineMeta
          }
  RT.RawDef
    { RT.geist = geist,
      RT.leadingComment = [],
      RT.body = defineMetaBody defineMeta,
      RT.trailingComment = [],
      RT.endLoc = defineMetaEndLoc defineMeta
    }

decDefineMetaBlock :: SE.Series (RawDefineMeta N.Name) -> D.Doc
decDefineMetaBlock defineMetaList = do
  D.join
    [ D.text "{",
      D.nest D.indent $ D.join [D.line, decDefineMetaItems $ SE.elems defineMetaList, C.asSuffix $ SE.trailingComment defineMetaList],
      D.line,
      D.text "}"
    ]

decDefineMetaItems :: [(C, RawDefineMeta N.Name)] -> D.Doc
decDefineMetaItems entries =
  case entries of
    [] ->
      D.Nil
    [(c, defineMeta)] ->
      decDefineMeta c defineMeta
    (c, defineMeta) : rest ->
      D.join [decDefineMeta c defineMeta, D.line, D.line, decDefineMetaItems rest]

decDataArgs :: Maybe (RT.Args RT.RawType) -> D.Doc
decDataArgs argsOrNone =
  case argsOrNone of
    Nothing ->
      D.Nil
    Just args -> do
      RT.decodeArgs' args

decConsInfo :: RawConsInfo BN.BaseName -> D.Doc
decConsInfo (RawConsInfo {name = consName, expArgs}) = do
  let consName' = D.text (BN.reify consName)
  D.join [consName', RT.decodeConsArgsMaybe expArgs]

decNominalGeist :: (NominalTag, RT.RawGeist BN.BaseName, Loc) -> D.Doc
decNominalGeist (tag, geist, _) = do
  let keyword = nominalTagToText tag
  let geistDoc = case tag of
        Define ->
          RT.decGeist (D.text . BN.reify) geist
        DestPassing ->
          RT.decGeist (D.text . BN.reify) geist
        DestPassingInline ->
          RT.decGeist (D.text . BN.reify) geist
        Inline ->
          RT.decGeist (D.text . BN.reify) geist
        Macro ->
          RT.decGeist (D.text . BN.reify) geist
        MacroInline ->
          RT.decGeist (D.text . BN.reify) geist
        Constant ->
          RT.decGeist (D.text . BN.reify) geist
        ConstantMeta ->
          RT.decGeist (D.text . BN.reify) geist
        Alias ->
          RT.decTypeGeist (D.text . BN.reify) geist
        AliasOpaque ->
          RT.decTypeGeist (D.text . BN.reify) geist
        Data ->
          RT.decTypeGeist (D.text . BN.reify) geist
        Resource ->
          RT.decTypeGeist (D.text . BN.reify) geist
  PI.arrange
    [ PI.horizontal $ D.text keyword,
      PI.inject geistDoc
    ]

attachStmtComment :: C -> D.Doc -> D.Doc
attachStmtComment c doc =
  D.join [C.asStmtPrefix c, doc]
