module Entity.RawProgram.Decode (pp) where

import Control.Comonad.Cofree
import Data.Bifunctor
import Data.Char
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.C
import Entity.C.Decode qualified as C
import Entity.DefiniteDescription qualified as DD
import Entity.Doc qualified as D
import Entity.ExternalName qualified as EN
import Entity.LocalLocator qualified as LL
import Entity.LowType.Decode qualified as LowType
import Entity.RawDecl qualified as RDE
import Entity.RawProgram
import Entity.RawTerm qualified as RT
import Entity.RawTerm.Decode qualified as RT
import Entity.Syntax.Series.Decode qualified as SE

pp :: (C, RawProgram) -> T.Text
pp (c1, RawProgram _ importOrNone c2 foreignOrNone c3 stmtList) = do
  let c1' = decHeadComment c1
  let importOrNone' = decImport importOrNone
  let foreignOrNone' = decForeign foreignOrNone
  let stmtList' = map (first decStmt) stmtList
  let program' = [(importOrNone', c2), (foreignOrNone', c3)] ++ stmtList'
  T.dropWhile isSpace $ D.layout $ D.join $ c1' : decTopDocList [] program'

decHeadComment :: C -> D.Doc
decHeadComment c =
  if null c
    then D.Nil
    else D.join $ commentToDoc c ++ [D.line]

attachComment :: C -> [D.Doc] -> [D.Doc]
attachComment c docList =
  if null c
    then docList
    else commentToDoc c ++ docList

decTopDocList :: C -> [(D.Doc, C)] -> [D.Doc]
decTopDocList c docList =
  case docList of
    [] ->
      attachComment c []
    (doc, c') : rest -> do
      attachComment c $ [doc, D.line, D.line] ++ decTopDocList c' rest

decImport :: Maybe RawImport -> D.Doc
decImport importOrNone =
  case importOrNone of
    Nothing ->
      D.Nil
    Just (RawImport _ _ (_, importItemList)) -> do
      let importItemList' = map decImportItem importItemList
      D.join
        [ D.text "import",
          D.text " {",
          D.join [D.line, D.listSeq importItemList'],
          D.line,
          D.text "}"
        ]

decImportItem :: (C, RawImportItem) -> D.Doc
decImportItem (_, RawImportItem _ _ (item, _) (args, _)) = do
  if null args
    then D.join [D.text item]
    else do
      let args' = map decImportItemLocator args
      D.join [D.text item, D.text " {", D.commaSeqH args', D.text "}"]

decImportItemLocator :: (C, ((a, LL.LocalLocator), C)) -> D.Doc
decImportItemLocator (_, ((_, l), _)) =
  D.text (LL.reify l)

decForeign :: Maybe RawForeign -> D.Doc
decForeign foreignOrNone =
  case foreignOrNone of
    Nothing ->
      D.Nil
    Just (RawForeign _ (_, foreignList)) -> do
      let foreignList' = map decForeignItem foreignList
      D.join
        [ D.text "foreign",
          D.text " {",
          D.join [D.line, D.listSeq foreignList'],
          D.line,
          D.text "}"
        ]

decForeignItem :: (C, RawForeignItem) -> D.Doc
decForeignItem (_, RawForeignItem funcName _ args _ (cod, _)) = do
  let args' = decArgList (LowType.decode . fst) args
  let cod' = LowType.decode cod
  D.join [D.text (EN.reify funcName), D.text "(", args', D.text "): ", cod']

decStmt :: RawStmt -> D.Doc
decStmt stmt =
  case stmt of
    RawStmtDefine _ _ decl (_, (body, _)) -> do
      let (functionName, _) = RDE.name decl
      let impArgs' = RT.decodeArgs' $ RDE.impArgs decl
      let expArgs' = RT.decodeArgs $ RDE.expArgs decl
      let (_, (cod, _)) = RDE.cod decl
      let cod' = RT.toDoc cod
      let body' = RT.toDoc body
      D.join
        [ D.text "define ",
          D.text (DD.localLocator functionName),
          impArgs',
          expArgs',
          D.text ": ",
          cod',
          D.text " {",
          D.nest D.indent $ D.join [D.line, body'],
          D.line,
          D.text "}"
        ]
    RawStmtDefineConst _ _ (name, _) (_, (cod, _)) (_, (body, _)) ->
      D.join
        [ D.text "constant ",
          D.text (DD.localLocator name),
          D.text ": ",
          RT.toDoc cod,
          D.text " {",
          D.nest D.indent $ D.join [D.line, RT.toDoc body],
          D.line,
          D.text "}"
        ]
    RawStmtDefineData _ _ (dataName, _) argsOrNone consInfo -> do
      let consInfo' = SE.decode $ fmap decConsInfo consInfo
      D.join
        [ D.text "data ",
          D.text (DD.localLocator dataName),
          decDataArgs argsOrNone,
          D.text " ",
          consInfo'
        ]
    RawStmtDefineResource _ m (name, _) _ (_, discarder) (_, copier) ->
      RT.toDoc $ m :< RT.Resource name [] discarder copier
    RawStmtDeclare _ _ _ declList -> do
      let declList' = decDeclList declList
      D.join
        [ D.text "declare ",
          D.text "{",
          D.line,
          D.listSeq declList',
          D.line,
          D.text "}"
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

commentToDoc :: C -> [D.Doc]
commentToDoc c = do
  foldr (\com acc -> [D.text "//", D.text com, D.line] ++ acc) [] c

decDeclList :: [(C, RDE.RawDecl)] -> [D.Doc]
decDeclList declList =
  case declList of
    [] ->
      []
    (_, decl) : rest -> do
      let (functionName, _) = RDE.name decl
      let impArgs' = RT.decodeArgs $ RDE.impArgs decl
      let expArgs' = RT.decodeArgs $ RDE.expArgs decl
      let (_, (cod, _)) = RDE.cod decl
      let cod' = RT.toDoc cod
      let decl' =
            D.join
              [ D.text (DD.localLocator functionName),
                impArgs',
                expArgs',
                D.text ": ",
                cod'
              ]
      decl' : decDeclList rest

decArgList :: (a -> D.Doc) -> ArgList a -> D.Doc
decArgList f (args, _) = do
  let args' = decArgList' f args
  if D.isSingle args'
    then D.commaSeqH args'
    else D.commaSeqV args'

decArgList' :: (a -> D.Doc) -> [(C, a)] -> [D.Doc]
decArgList' f args =
  case args of
    [] ->
      []
    (_, a) : rest -> do
      f a : decArgList' f rest
