module Language.RawTerm.Rule.RawStmt.ToDoc (pp, ImportInfo (..)) where

import BasicPrettyPrinter.Rule.Doc qualified as D
import BasicPrettyPrinter.Rule.Piece qualified as PI
import Control.Monad
import Data.Bifunctor
import Data.Text qualified as T
import Kernel.Rule.UnusedGlobalLocators (UnusedGlobalLocators, isUsedGL)
import Kernel.Rule.UnusedLocalLocators (UnusedLocalLocators, isUsedLL)
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.ForeignCodType qualified as FCT
import Language.Common.Rule.LocalLocator qualified as LL
import Language.Common.Rule.Opacity qualified as O
import Language.Common.Rule.StmtKind qualified as SK
import Language.RawTerm.Rule.Name qualified as N
import Language.RawTerm.Rule.RawStmt
import Language.RawTerm.Rule.RawTerm qualified as RT
import Language.RawTerm.Rule.RawTerm.ToDoc qualified as RT
import Logger.Rule.Hint
import SyntaxTree.Rule.C
import SyntaxTree.Rule.Series (Series (hasOptionalSeparator))
import SyntaxTree.Rule.Series qualified as SE
import SyntaxTree.Rule.Series.ToDoc qualified as SE

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

decImport :: ImportInfo -> RawImport -> Maybe D.Doc
decImport importInfo importStmt = do
  if isImportEmpty importStmt
    then Nothing
    else do
      let (RawImport c _ importItemList _) = importStmt
      let importItemList' = SE.compressEither $ fmap (filterImport importInfo) importItemList
      let importItemList'' = SE.assoc $ decImportItem <$> sortImport importItemList'
      if SE.isEmpty importItemList''
        then Nothing
        else do
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
filterUnused importInfo rawImportItem = do
  case rawImportItem of
    RawStaticKey {} ->
      return rawImportItem
    RawImportItem m (loc, c) lls -> do
      if isUsedGL (unusedGlobalLocators importInfo) loc
        then do
          let lls' = SE.filter (isUsedLL (unusedLocalLocators importInfo) . snd) lls
          return $ RawImportItem m (loc, c) lls'
        else Left c

filterPreset :: ImportInfo -> RawImportItem -> Either C RawImportItem
filterPreset importInfo item = do
  case item of
    RawStaticKey {} ->
      return item
    RawImportItem m (loc, c) lls -> do
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
  nubLocalLocators . sortLocalLocators <$> series' {SE.elems = mergeAdjacentImport (SE.elems series')}

mergeAdjacentImport :: [(C, RawImportItem)] -> [(C, RawImportItem)]
mergeAdjacentImport importList = do
  case importList of
    [] ->
      []
    [item] ->
      [item]
    (c1, item1) : (c2, item2) : rest -> do
      case (item1, item2) of
        (RawStaticKey m1 c1' ks1, RawStaticKey _ c2' ks2) -> do
          let item = RawStaticKey m1 (c1' ++ c2') (SE.appendLeftBiased ks1 ks2)
          mergeAdjacentImport $ (c1 ++ c2, item) : rest
        (RawImportItem m1 (locator1, c1') localLocatorList1, RawImportItem _ (locator2, c2') localLocatorList2)
          | locator1 == locator2 -> do
              let localLocatorList = SE.appendLeftBiased localLocatorList1 localLocatorList2
              let item = RawImportItem m1 (locator1, c1' ++ c2') localLocatorList
              mergeAdjacentImport $ (c1 ++ c2, item) : rest
        _ ->
          (c1, item1) : mergeAdjacentImport ((c2, item2) : rest)

sortLocalLocators :: RawImportItem -> RawImportItem
sortLocalLocators rawImportItem = do
  case rawImportItem of
    RawImportItem m locator localLocators -> do
      let cmp (_, x) (_, y) = compare x y
      RawImportItem m locator $ SE.sortSeriesBy cmp localLocators
    RawStaticKey m c ks -> do
      let cmp (_, x) (_, y) = compare x y
      RawStaticKey m c $ SE.sortSeriesBy cmp ks

nubLocalLocators :: RawImportItem -> RawImportItem
nubLocalLocators rawImportItem = do
  case rawImportItem of
    RawImportItem m locator localLocators -> do
      let eq (_, x) (_, y) = x == y
      RawImportItem m locator $ SE.nubSeriesBy eq localLocators
    RawStaticKey m c ks -> do
      let eq (_, x) (_, y) = x == y
      RawStaticKey m c $ SE.nubSeriesBy eq ks

decImportItem :: RawImportItem -> (D.Doc, C)
decImportItem rawImportItem = do
  case rawImportItem of
    RawImportItem _ (item, c) args -> do
      if SE.isEmpty args
        then (D.join [D.text item], c)
        else do
          let args' = SE.pushComment c args
          let args'' = SE.decode $ fmap decImportItemLocator args'
          (D.join [D.text item, D.text " ", args''], [])
    RawStaticKey _ c ks -> do
      if SE.isEmpty ks
        then (D.Nil, c)
        else do
          let ks' = D.text . snd <$> SE.pushComment c ks
          (D.join [D.text "static", D.text " ", SE.decode ks'], [])

decImportItemLocator :: (a, LL.LocalLocator) -> D.Doc
decImportItemLocator (_, l) =
  D.text (LL.reify l)

decStmt :: RawStmt -> D.Doc
decStmt stmt =
  case stmt of
    RawStmtDefine c stmtKind def -> do
      case stmtKind of
        SK.Normal O.Clear ->
          RT.decodeDef (RT.nameToDoc . N.Var) "inline" c (fmap BN.reify def)
        SK.Main O.Clear _ ->
          RT.decodeDef (RT.nameToDoc . N.Var) "inline" c (fmap BN.reify def)
        _ ->
          RT.decodeDef (RT.nameToDoc . N.Var) "define" c (fmap BN.reify def)
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
            SE.decode $ fmap (decTopGeist . fst) geistList
          ]
    RawStmtForeign c foreignList -> do
      let foreignList' = SE.decode $ fmap decForeignItem foreignList
      RT.attachComment c $
        D.join
          [ D.text "foreign ",
            foreignList'
          ]

decForeignItem :: RawForeignItem -> D.Doc
decForeignItem (RawForeignItemF _ funcName _ args _ _ cod) = do
  let args' = SE.decode $ fmap RT.toDoc args
  let cod' =
        case cod of
          FCT.Cod c ->
            RT.toDoc c
          FCT.Void ->
            D.text "void"
  D.join [D.text (EN.reify funcName), args', D.text ": ", cod']

decDataArgs :: Maybe (RT.Args RT.RawTerm) -> D.Doc
decDataArgs argsOrNone =
  case argsOrNone of
    Nothing ->
      D.Nil
    Just args -> do
      RT.decodeArgs' args

decConsInfo :: RawConsInfo BN.BaseName -> D.Doc
decConsInfo (_, consName, isConstLike, args, _) = do
  let consName' = D.text (BN.reify consName)
  if isConstLike
    then D.join [consName']
    else D.join [consName', RT.decodeArgs (args, [])]

decTopGeist :: RT.TopGeist -> D.Doc
decTopGeist = do
  RT.decGeist (D.text . BN.reify)
