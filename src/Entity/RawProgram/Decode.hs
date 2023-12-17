module Entity.RawProgram.Decode (pp) where

import Control.Comonad.Cofree
import Data.Bifunctor
import Data.Char
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.C
import Entity.DefiniteDescription qualified as DD
import Entity.Doc qualified as D
import Entity.ExternalName qualified as EN
import Entity.LocalLocator qualified as LL
import Entity.LowType.Decode qualified as LowType
import Entity.RawBinder
import Entity.RawDecl (ExpArgs)
import Entity.RawDecl qualified as RDE
import Entity.RawIdent
import Entity.RawProgram
import Entity.RawTerm qualified as RT
import Entity.RawTerm.Decode qualified as RT

pp :: (C, RawProgram) -> T.Text
pp (c1, RawProgram _ importOrNone c2 foreignOrNone c3 stmtList) = do
  let importOrNone' = decImport importOrNone
  let foreignOrNone' = decForeign foreignOrNone
  let stmtList' = map (first decStmt) stmtList
  let program' = [(importOrNone', c2), (foreignOrNone', c3)] ++ stmtList'
  T.dropWhile isSpace $ D.layout $ D.join $ decTopDocList c1 program'

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
      let (impArgs, _) = RDE.impArgs decl
      let impArgs' = decImpArgs impArgs
      let (c3, (expArgs, _)) = RDE.expArgs decl
      let expArgs' = decExpArgs (c3, expArgs)
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
    RawStmtDefineData _ _ (dataName, _) argsOrNone _ consInfo -> do
      let consInfo' = map decConsInfo consInfo
      D.join
        [ D.text "data ",
          D.text (DD.localLocator dataName),
          decDataArgs argsOrNone,
          D.text " {",
          D.join [D.line, D.listSeq consInfo'],
          D.line,
          D.text "}"
        ]
    RawStmtDefineResource {} ->
      D.text "resource"
    RawStmtDeclare {} ->
      D.text "declare"

decImpArgs :: [(C, RawBinder (RT.RawTerm, C))] -> D.Doc
decImpArgs impArgs =
  if null impArgs
    then D.Nil
    else do
      let impArgs' = decImpArgSeq impArgs
      if D.isSingle impArgs'
        then D.join [D.text "<", D.commaSeqH impArgs', D.text ">"]
        else
          D.join
            [ D.text "<",
              D.nest D.indent $ D.join [D.line, D.commaSeqV impArgs'],
              D.line,
              D.text ">"
            ]

decImpArgSeq :: [(C, RawBinder (RT.RawTerm, C))] -> [D.Doc]
decImpArgSeq impArgs =
  case impArgs of
    [] ->
      []
    (_, (_, x, _, _, (t, _))) : rest -> do
      case t of
        _ :< RT.Hole {} ->
          D.text x : decImpArgSeq rest
        _ -> do
          let t' = RT.toDoc t
          if D.isSingle [t']
            then D.join [D.text x, D.text ": ", t'] : decImpArgSeq rest
            else D.join [D.text x, D.text ": ", D.line, t'] : decImpArgSeq rest

decExpArgs :: (Maybe (C, C), [(C, RawBinder (RT.RawTerm, C))]) -> D.Doc
decExpArgs (ofOrNone, expArgs) = do
  let expArgs' = decExpArgSeq expArgs
  case ofOrNone of
    Nothing -> do
      if D.isSingle expArgs'
        then D.join [D.text "(", D.commaSeqH expArgs', D.text ")"]
        else decExpArgs (Just ([], []), expArgs)
    Just _ ->
      D.join
        [ D.text " of ",
          D.text "{",
          D.line,
          D.listSeq expArgs',
          D.line,
          D.text "}"
        ]

decExpArgSeq :: [(C, RawBinder (RT.RawTerm, C))] -> [D.Doc]
decExpArgSeq expArgs =
  case expArgs of
    [] ->
      []
    (_, (_, x, _, _, (t, _))) : rest -> do
      let t' = RT.toDoc t
      if isHole x
        then t' : decExpArgSeq rest
        else do
          if D.isSingle [t']
            then D.join [D.text x, D.text ": ", t'] : decExpArgSeq rest
            else D.join [D.text x, D.text ": ", D.line, t'] : decExpArgSeq rest

decDataArgs :: Maybe ExpArgs -> D.Doc
decDataArgs argsOrNone =
  case argsOrNone of
    Nothing ->
      D.Nil
    Just (_, (args, _)) -> do
      D.join [D.text "(", D.commaSeqH $ map decDataArg args, D.text ")"]

decDataArg :: (C, RawBinder (RT.RawTerm, C)) -> D.Doc
decDataArg (_, (_, x, _, _, (t, _))) = do
  let x' = D.text x
  case t of
    _ :< RT.Hole {} ->
      x'
    _ -> do
      D.join [x', RT.typeAnnot t]

decConsInfo :: (C, RawConsInfo) -> D.Doc
decConsInfo (_, (_, (consName, _), isConstLike, args)) = do
  let consName' = D.text (BN.reify consName)
  if isConstLike
    then consName'
    else do
      case args of
        (Just (_, _), (args', _)) ->
          D.join
            [ consName',
              D.text " of ",
              D.text "{",
              D.line,
              D.listSeq $ map decRawArg args',
              D.line,
              D.text "}"
            ]
        (Nothing, (args', _)) ->
          D.join [consName', D.text "(", D.commaSeqH $ map decRawArg args', D.text ")"]

decRawArg :: (C, RawBinder (RT.RawTerm, C)) -> D.Doc
decRawArg (_, (_, x, _, _, (t, _))) = do
  let t' = RT.toDoc t
  if isHole x
    then t'
    else do
      let x' = D.text x
      D.join [x', RT.typeAnnot t]

commentToDoc :: C -> [D.Doc]
commentToDoc c = do
  foldr (\com acc -> [D.text "//", D.text com, D.line] ++ acc) [] c

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
