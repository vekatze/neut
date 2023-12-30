module Entity.RawProgram.Decode (pp) where

import Data.Bifunctor
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.C
import Entity.C.Decode qualified as C
import Entity.Doc qualified as D
import Entity.ExternalName qualified as EN
import Entity.LocalLocator qualified as LL
import Entity.Opacity qualified as O
import Entity.Piece qualified as PI
import Entity.RawLowType.Decode qualified as RLT
import Entity.RawProgram
import Entity.RawTerm qualified as RT
import Entity.RawTerm.Decode qualified as RT
import Entity.StmtKind qualified as SK
import Entity.Syntax.Series qualified as SE
import Entity.Syntax.Series.Decode qualified as SE

pp :: (C, RawProgram) -> T.Text
pp (c1, RawProgram _ importOrNone c2 foreignOrNone c3 stmtList) = do
  let importOrNone' = fmap decImport importOrNone
  let foreignOrNone' = fmap decForeign foreignOrNone
  let stmtList' = map (first (Just . decStmt)) stmtList
  let program' = [(importOrNone', c2), (foreignOrNone', c3)] ++ stmtList'
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

decImport :: RawImport -> D.Doc
decImport (RawImport c _ importItemList) = do
  RT.attachComment c $
    D.join
      [ D.text "import ",
        SE.decode $ SE.assoc $ fmap decImportItem importItemList
      ]

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

decForeign :: RawForeign -> D.Doc
decForeign (RawForeign c foreignList) = do
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
    RawStmtDefineData c1 _ (dataName, c2) argsOrNone consInfo -> do
      RT.attachComment (c1 ++ c2) $
        D.join
          [ D.text "data ",
            D.text (BN.reify dataName),
            decDataArgs argsOrNone,
            D.text " ",
            SE.decode $ fmap decConsInfo consInfo
          ]
    RawStmtDefineResource c1 _ (name, c2) c3 discarder copier -> do
      let resourcePair = SE.pushComment c3 $ SE.fromListWithComment SE.Brace SE.Hyphen [discarder, copier]
      RT.attachComment (c1 ++ c2) $
        PI.arrange
          [ PI.horizontal $ D.text "resource",
            PI.horizontal $ D.text (BN.reify name),
            PI.inject $ SE.decode $ fmap RT.toDoc resourcePair
          ]
    RawStmtDeclare c _ declList -> do
      RT.attachComment c $
        D.join
          [ D.text "declare ",
            SE.decode $ fmap decDeclList declList
          ]

decDataArgs :: Maybe (RT.Args RT.RawTerm) -> D.Doc
decDataArgs argsOrNone =
  case argsOrNone of
    Nothing ->
      D.Nil
    Just args -> do
      RT.decodeArgs' args

decConsInfo :: RawConsInfo BN.BaseName -> D.Doc
decConsInfo (_, (consName, cCons), isConstLike, args) = do
  let consName' = D.text (BN.reify consName)
  if isConstLike
    then D.join [consName', C.asSuffix cCons]
    else D.join [consName', C.asSuffix cCons, RT.decodeArgs (args, [])]

decDeclList :: RT.TopDefHeader -> D.Doc
decDeclList decl = do
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
