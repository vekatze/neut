module Entity.RawProgram.Decode (pp, ImportInfo (..)) where

import Control.Monad
import Data.Bifunctor
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.C
import Entity.C.Decode qualified as C
import Entity.Doc qualified as D
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Entity.Opacity qualified as O
import Entity.Piece qualified as PI
import Entity.RawLowType.Decode qualified as RLT
import Entity.RawProgram
import Entity.RawTerm qualified as RT
import Entity.RawTerm.Decode qualified as RT
import Entity.StmtKind qualified as SK
import Entity.Syntax.Series (Series (hasOptionalSeparator))
import Entity.Syntax.Series qualified as SE
import Entity.Syntax.Series.Decode qualified as SE
import Entity.UnusedGlobalLocators (UnusedGlobalLocators, isUsedGL)
import Entity.UnusedLocalLocators (UnusedLocalLocators, isUsedLL)

data ImportInfo = ImportInfo
  { presetNames :: [(T.Text, [BN.BaseName])], -- "prelude"
    unusedLocalLocators :: UnusedLocalLocators,
    unusedGlobalLocators :: UnusedGlobalLocators
  }

pp :: ImportInfo -> (C, RawProgram) -> T.Text
pp importInfo (c1, RawProgram _ importOrNone c2 stmtList) = do
  let importOrNone' = decImport importInfo importOrNone
  let stmtList' = map (first (Just . decStmt)) stmtList
  let program' = (importOrNone', c2) : stmtList'
  D.layout $ decTopDocList c1 program'

decTopDocList :: C -> [(Maybe D.Doc, C)] -> D.Doc
decTopDocList c docList =
  case docList of
    [] ->
      RT.attachComment c D.Nil
    [(Nothing, c')] ->
      RT.attachComment (c ++ c') D.Nil
    [(Just doc, c')] ->
      if null c'
        then RT.attachComment c $ D.join [doc, D.line]
        else RT.attachComment c $ D.join [doc, D.line, D.line, decTopDocList c' []]
    (Nothing, c') : rest ->
      decTopDocList (c ++ c') rest
    (Just doc, c') : rest -> do
      RT.attachComment c $ D.join [doc, D.line, D.line, decTopDocList c' rest]

decImport :: ImportInfo -> Maybe RawImport -> Maybe D.Doc
decImport importInfo importOrNone = do
  (RawImport c _ importItemList _) <- importOrNone
  let importItemList' = SE.compressEither $ fmap (filterImport importInfo) importItemList
  return $
    RT.attachComment c $
      D.join
        [ D.text "import ",
          SE.decode $ SE.assoc $ decImportItem <$> sortImport importItemList'
        ]

filterImport :: ImportInfo -> RawImportItem -> Either C RawImportItem
filterImport importInfo = do
  filterUnused importInfo >=> filterPreset importInfo

filterUnused :: ImportInfo -> RawImportItem -> Either C RawImportItem
filterUnused importInfo (RawImportItem m (loc, c) lls) = do
  if isUsedGL (unusedGlobalLocators importInfo) loc
    then do
      let lls' = SE.filter (isUsedLL (unusedLocalLocators importInfo) . snd) lls
      return $ RawImportItem m (loc, c) lls'
    else Left c

filterPreset :: ImportInfo -> RawImportItem -> Either C RawImportItem
filterPreset importInfo item@(RawImportItem m (loc, c) lls) = do
  case lookup loc (presetNames importInfo) of
    Nothing ->
      return item
    Just names -> do
      if SE.isEmpty lls
        then Left c
        else do
          let lls' = SE.catMaybes $ fmap (filterLocalLocator names) lls
          if SE.isEmpty lls'
            then Left c
            else return $ RawImportItem m (loc, c) lls'

filterLocalLocator :: [BN.BaseName] -> (Hint, LL.LocalLocator) -> Maybe (Hint, LL.LocalLocator)
filterLocalLocator names (m, ll) =
  if LL.baseName ll `elem` names
    then Nothing
    else return (m, ll)

sortImport :: SE.Series RawImportItem -> SE.Series RawImportItem
sortImport series = do
  let series' = SE.sortSeriesBy compareImportItem series
  sortLocalLocators <$> series' {SE.elems = mergeAdjacentImport (SE.elems series')}

mergeAdjacentImport :: [(C, RawImportItem)] -> [(C, RawImportItem)]
mergeAdjacentImport importList = do
  case importList of
    [] ->
      []
    [item] ->
      [item]
    (c1, item1) : (c2, item2) : rest -> do
      let RawImportItem m1 (locator1, c1') localLocatorList1 = item1
      let RawImportItem _ (locator2, c2') localLocatorList2 = item2
      if locator1 /= locator2
        then (c1, item1) : mergeAdjacentImport ((c2, item2) : rest)
        else do
          let item = RawImportItem m1 (locator1, c1' ++ c2') (SE.appendLeftBiased localLocatorList1 localLocatorList2)
          mergeAdjacentImport $ (c1 ++ c2, item) : rest

sortLocalLocators :: RawImportItem -> RawImportItem
sortLocalLocators (RawImportItem m locator localLocators) = do
  let cmp (_, x) (_, y) = compare x y
  RawImportItem m locator $ SE.sortSeriesBy cmp localLocators

decImportItem :: RawImportItem -> (D.Doc, C)
decImportItem (RawImportItem _ (item, c) args) = do
  if SE.isEmpty args
    then (D.join [D.text item], c)
    else do
      let args' = SE.pushComment c args
      let args'' = SE.decode $ fmap decImportItemLocator args'
      (D.join [D.text item, D.text " ", args''], [])

decImportItemLocator :: (a, LL.LocalLocator) -> D.Doc
decImportItemLocator (_, l) =
  D.text (LL.reify l)

decStmt :: RawStmt -> D.Doc
decStmt stmt =
  case stmt of
    RawStmtDefine c stmtKind def -> do
      case stmtKind of
        SK.Normal O.Clear ->
          RT.decodeDef "inline" c (fmap BN.reify def)
        _ ->
          RT.decodeDef "define" c (fmap BN.reify def)
    RawStmtDefineConst c1 _ (name, c2) cod body -> do
      let constClause = RT.mapKeywordClause RT.toDoc (cod, body)
      RT.attachComment (c1 ++ c2) $
        D.join
          [ D.text "constant ",
            D.text (BN.reify name),
            RT.decodeKeywordClause ":" constClause
          ]
    RawStmtDefineData c1 _ (dataName, c2) argsOrNone consInfo _ -> do
      RT.attachComment (c1 ++ c2) $
        D.join
          [ D.text "data ",
            D.text (BN.reify dataName),
            decDataArgs argsOrNone,
            D.text " ",
            SE.decode $ fmap decConsInfo consInfo
          ]
    RawStmtDefineResource c1 _ (name, c2) discarder copier trailingComment -> do
      let series =
            SE.Series
              { elems = [discarder, copier],
                trailingComment,
                prefix = Nothing,
                container = Just SE.Brace,
                separator = SE.Comma,
                hasOptionalSeparator = True
              }
      -- let resourcePair = SE.pushComment c3 $ SE.fromListWithComment SE.Brace SE.Hyphen [discarder, copier]
      RT.attachComment (c1 ++ c2) $
        PI.arrange
          [ PI.horizontal $ D.text "resource",
            PI.horizontal $ D.text (BN.reify name),
            PI.inject $ SE.decode $ fmap RT.toDoc series
          ]
    RawStmtNominal c _ geistList -> do
      RT.attachComment c $
        D.join
          [ D.text "nominal ",
            SE.decode $ fmap decGeistList geistList
          ]
    RawStmtForeign c foreignList -> do
      let foreignList' = SE.decode $ fmap decForeignItem foreignList
      RT.attachComment c $
        D.join
          [ D.text "foreign ",
            foreignList'
          ]

decForeignItem :: RawForeignItem -> D.Doc
decForeignItem (RawForeignItem _ funcName _ args _ _ cod) = do
  let args' = SE.decode $ fmap RLT.decode args
  let cod' = RLT.decode cod
  D.join [D.text (EN.reify funcName), args', D.text ": ", cod']

decDataArgs :: Maybe (RT.Args RT.RawTerm) -> D.Doc
decDataArgs argsOrNone =
  case argsOrNone of
    Nothing ->
      D.Nil
    Just args -> do
      RT.decodeArgs' args

decConsInfo :: RawConsInfo BN.BaseName -> D.Doc
decConsInfo (_, (consName, cCons), isConstLike, args, _) = do
  let consName' = D.text (BN.reify consName)
  if isConstLike
    then D.join [consName', C.asSuffix cCons]
    else D.join [consName', C.asSuffix cCons, RT.decodeArgs (args, [])]

decGeistList :: (RT.TopGeist, a) -> D.Doc
decGeistList (decl, _) = do
  let (functionName, _) = RT.name decl
  let impArgs' = RT.decodeArgs' $ RT.impArgs decl
  let cod = RT.toDoc $ snd $ RT.cod decl
  if RT.isConstLike decl
    then do
      D.join
        [ D.text (BN.reify functionName),
          impArgs',
          D.text ": ",
          cod
        ]
    else do
      let expArgs' = RT.decodeArgs' $ RT.expArgs decl
      D.join
        [ D.text (BN.reify functionName),
          impArgs',
          expArgs',
          D.text ": ",
          cod
        ]
