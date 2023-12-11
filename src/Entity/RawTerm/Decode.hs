module Entity.RawTerm.Decode (pp) where

import Control.Comonad.Cofree
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.C
import Entity.DefiniteDescription qualified as DD
import Entity.Doc qualified as D
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.Locator qualified as Locator
import Entity.LowType qualified as LT
import Entity.Magic qualified as M
import Entity.Name qualified as N
import Entity.PrimNumSize.ToInt qualified as PNS
import Entity.PrimOp qualified as P
import Entity.PrimOp.BinaryOp qualified as BinaryOp
import Entity.PrimOp.CmpOp qualified as CmpOp
import Entity.PrimOp.UnaryOp qualified as UnaryOp
import Entity.PrimType qualified as PT
import Entity.RawBinder
import Entity.RawIdent
import Entity.RawPattern qualified as RP
import Entity.RawTerm
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV

pp :: RawTerm -> T.Text
pp e = do
  D.layout $ toDoc e

toDoc :: RawTerm -> D.Doc
toDoc term =
  case term of
    _ :< Tau ->
      D.text "tau"
    _ :< Var varOrLocator ->
      nameToDoc varOrLocator
    _ :< Pi _ impArgs _ _ expArgs _ _ cod -> do
      let impArgs' = impArgsToDoc $ map f impArgs
      let expArgs' = expPiArgsToDoc $ map f expArgs
      let cod' = toDoc cod
      let arrow = if isMultiLine [impArgs', expArgs'] then D.join [D.line, D.text "->"] else D.text " -> "
      if isMultiLine [cod']
        then D.join [impArgs', expArgs', arrow, D.line, cod']
        else D.join [impArgs', expArgs', arrow, cod']
    _ :< PiIntro impArgs expArgs body -> do
      let impArgs' = impArgsToDoc impArgs
      let expArgs' = expPiIntroArgsToDoc expArgs
      D.join [impArgs', expArgs', D.text " => ", clauseBodyToDoc body]
    _ :< PiIntroFix (_, k) impArgs expArgs cod body -> do
      let impArgs' = impArgsToDoc impArgs
      let expArgs' = expPiIntroArgsToDoc expArgs
      D.join [D.text "define ", D.text k, impArgs', expArgs', typeAnnot cod, D.text " ", recBody body]
    _ :< PiElim isExplicit e args -> do
      let prefix = if isExplicit then D.text "call " else D.Nil
      let e' = toDoc e
      let args' = map toDoc args
      D.join [prefix, piElimToDoc e' args']
    _ :< PiElimByKey isExplicit name kvs -> do
      let prefix = if isExplicit then D.text "call " else D.Nil
      let kvs' = map (\(_, k, v) -> (k, v)) kvs
      D.join [prefix, piElimKeyToDoc name kvs']
    _ :< PiElimExact e ->
      D.join [D.text "exact ", toDoc e]
    _ :< Data _ dd _ ->
      D.text $ DD.reify dd
    _ :< DataIntro _ dd _ _ ->
      D.text $ DD.reify dd
    _ :< DataElim isNoetic es patternRowList -> do
      let keyword = if isNoetic then D.text "case" else D.text "match"
      let es' = map toDoc es
      let patternRowList' = decodePatternRowList $ RP.toList patternRowList
      D.join
        [ keyword,
          D.text " ",
          commaSeqH es',
          D.text " {",
          D.join [D.line, listSeq patternRowList'],
          D.line,
          D.text "}"
        ]
    _ :< Noema t ->
      D.join [D.text "&", toDoc t]
    _ :< Embody e ->
      D.join [D.text "*", toDoc e]
    _ :< Let letKind mxt noeticVarList e cont -> do
      let keyword =
            case letKind of
              Plain -> "let"
              Noetic -> "tie"
              Try -> "try"
              Bind -> "bind"
      let mxt' = letArgToDoc mxt
      let noeticVarList' = decodeNoeticVarList noeticVarList
      let e' = toDoc e
      let cont' = toDoc cont
      if isMultiLine [mxt']
        then do
          let mxt'' = D.nest D.indent (D.join [D.line, mxt'])
          let e'' = D.nest D.indent e'
          D.join [D.text keyword, mxt'', noeticVarList', D.text " = ", e'', D.line, D.text "in", D.line, cont']
        else do
          if isMultiLine [e']
            then do
              let e'' = D.nest D.indent (D.join [D.line, e'])
              D.join
                [ D.text keyword,
                  D.text " ",
                  mxt',
                  noeticVarList',
                  D.text " =",
                  e'',
                  D.line,
                  D.text "in",
                  D.line,
                  cont'
                ]
            else
              D.join
                [ D.text keyword,
                  D.text " ",
                  mxt',
                  noeticVarList',
                  D.text " = ",
                  e',
                  D.text " in",
                  D.line,
                  cont'
                ]
    _ :< Prim prim ->
      case prim of
        WP.Type t ->
          primTypeToDoc t
        WP.Value v ->
          case v of
            WPV.Int _ x ->
              D.text (T.pack (show x))
            WPV.Float _ x ->
              D.text (T.pack (show x))
            WPV.Op op -> do
              decodePrimOp op
            WPV.StaticText _ txt ->
              D.text (T.pack (show txt))
    _ :< Magic magic ->
      case magic of
        M.Cast from to e -> do
          let from' = toDoc from
          let to' = toDoc to
          let e' = toDoc e
          D.join [D.text "magic ", piElimToDoc (D.text "cast") [from', to', e']]
        M.Store lt value pointer -> do
          let lt' = lowTypeToDoc lt
          let value' = toDoc value
          let pointer' = toDoc pointer
          D.join [D.text "magic ", piElimToDoc (D.text "store") [lt', value', pointer']]
        M.Load lt pointer -> do
          let lt' = lowTypeToDoc lt
          let pointer' = toDoc pointer
          D.join [D.text "magic ", piElimToDoc (D.text "load") [lt', pointer']]
        M.External _ _ funcName args varArgs -> do
          let funcName' = D.text $ T.pack (show $ EN.reify funcName)
          let args' = map toDoc args
          let varArgs' = map (\(lt, e) -> D.join [toDoc e, D.text " ", lowTypeToDoc lt]) varArgs
          if null varArgs'
            then D.join [D.text "magic ", piElimToDoc (D.text "external") (funcName' : args')]
            else D.join [D.text "magic ", piElimToDoc (D.text "external") (funcName' : args' ++ D.text "; " : varArgs')]
        M.Global lt name -> do
          let lt' = lowTypeToDoc lt
          let name' = D.text $ T.pack (show $ EN.reify name)
          D.join [D.text "magic ", piElimToDoc (D.text "global") [name', lt']]
    _ :< Hole {} ->
      D.text "_"
    _ :< Annotation {} -> do
      D.text "<annot>"
    _ :< Resource name discarder copier -> do
      let resourcePair = listSeq [toDoc discarder, toDoc copier]
      D.join [D.text "resource", D.text (DD.reify name), D.text "{", D.line, resourcePair, D.line, D.text "}"]
    _ :< Use trope args cont -> do
      let trope' = toDoc trope
      let args' = map (\(_, x, _, _, _) -> D.text x) args
      let cont' = toDoc cont
      if isMultiLine [trope']
        then
          D.join
            [ D.text "use",
              D.nest D.indent $
                D.join
                  [ D.line,
                    trope',
                    D.line,
                    D.text "{",
                    commaSeqH args',
                    D.text "}"
                  ],
              D.line,
              D.text "in",
              D.line,
              cont'
            ]
        else D.join [D.text "use ", trope', D.text " {", commaSeqH args', D.text "} in", D.line, cont']
    _ :< If ifCond ifBody elseIfList elseBody -> do
      D.join
        [ D.text "if ",
          toDoc ifCond,
          D.text " {",
          D.nest D.indent $ D.join [D.line, toDoc ifBody],
          D.line,
          D.text "}",
          decodeElseIfList elseIfList,
          D.text " else {",
          D.nest D.indent $ D.join [D.line, toDoc elseBody],
          D.line,
          D.text "}"
        ]
    _ :< When cond body -> do
      D.join
        [ D.text "when ",
          toDoc cond,
          D.text " {",
          D.nest D.indent $ D.join [D.line, toDoc body],
          D.line,
          D.text "}"
        ]
    _ :< Seq e1 e2 -> do
      D.join [toDoc e1, D.text ";", D.line, toDoc e2]
    _ :< ListIntro es -> do
      let es' = map toDoc es
      if isMultiLine es'
        then D.join [D.text "[", D.nest D.indent $ D.join [D.line, commaSeqV es'], D.line, D.text "]"]
        else D.join [D.text "[", commaSeqH es', D.text "]"]
    _ :< Admit ->
      D.text "admit"
    _ :< Detach e -> do
      let e' = toDoc e
      D.join [D.text "detach {", D.nest D.indent $ D.join [D.line, e'], D.line, D.text "}"]
    _ :< Attach e -> do
      let e' = toDoc e
      D.join [D.text "attach {", D.nest D.indent $ D.join [D.line, e'], D.line, D.text "}"]
    _ :< Option t -> do
      D.join [D.text "?", toDoc t]
    _ :< Assert (_, message) e -> do
      D.join
        [ D.text "assert ",
          D.text (T.pack (show message)),
          D.text " {",
          D.nest D.indent $ D.join [D.line, toDoc e],
          D.line,
          D.text "}"
        ]
    _ :< Introspect key clauseList -> do
      D.join
        [ D.text "introspect ",
          D.text key,
          D.text " {",
          D.join [D.line, listSeq $ map decodeIntrospectClause clauseList],
          D.line,
          D.text "}"
        ]
    _ :< With binder body -> do
      let binder' = toDoc binder
      let body' = toDoc body
      D.join
        [ D.text "with ",
          binder',
          D.text " {",
          D.nest D.indent $ D.join [D.line, body'],
          D.line,
          D.text "}"
        ]

decodeNoeticVarList :: [(Hint, RawIdent)] -> D.Doc
decodeNoeticVarList vs =
  if null vs
    then D.Nil
    else D.join [D.text " on ", commaSeqH (map (D.text . snd) vs)]

decodeElseIfList :: [(RawTerm, RawTerm)] -> D.Doc
decodeElseIfList elseIfList =
  case elseIfList of
    [] ->
      D.text ""
    (elseIfCond, elseIfBody) : rest ->
      D.join
        [ D.text " else-if ",
          toDoc elseIfCond,
          D.text " {",
          D.nest D.indent $ D.join [D.line, toDoc elseIfBody],
          D.line,
          D.text "}",
          decodeElseIfList rest
        ]

piArgToDoc :: RawBinder RawTerm -> D.Doc
piArgToDoc (_, x, _, _, t) = do
  let t' = toDoc t
  if isHole x
    then t'
    else do
      let x' = D.text x
      D.join [x', typeAnnot t]

piIntroArgToDoc :: RawBinder RawTerm -> D.Doc
piIntroArgToDoc (_, x, _, _, t) = do
  let x' = D.text x
  case t of
    _ :< Hole {} ->
      x'
    _ -> do
      D.join [x', typeAnnot t]

letArgToDoc :: (a, RP.RawPattern, C, C, RawTerm) -> D.Doc
letArgToDoc (_, x, _, _, t) = do
  let x' = decodePattern x
  case t of
    _ :< Hole {} ->
      x'
    _ -> do
      D.join [x', typeAnnot t]

typeAnnot :: RawTerm -> D.Doc
typeAnnot t = do
  let t' = toDoc t
  if isMultiLine [t']
    then D.join [D.text ":", D.line, t']
    else D.join [D.text ": ", t']

argsToDoc :: (RawBinder RawTerm -> D.Doc) -> [RawBinder RawTerm] -> D.Doc
argsToDoc argToDoc args = do
  let args' = map argToDoc args
  if isMultiLine args'
    then commaSeqV args'
    else commaSeqH args'

piArgsToDoc :: [RawBinder RawTerm] -> D.Doc
piArgsToDoc expArgs = do
  argsToDoc piArgToDoc expArgs

piIntroArgsToDoc :: [RawBinder RawTerm] -> D.Doc
piIntroArgsToDoc = do
  argsToDoc piIntroArgToDoc

impArgsToDoc :: [RawBinder RawTerm] -> D.Doc
impArgsToDoc impArgs = do
  if null impArgs
    then D.Nil
    else do
      let impArgs' = piIntroArgsToDoc impArgs
      if isMultiLine [impArgs']
        then do
          D.join [D.text "<", D.line, D.nest D.indent impArgs', D.line, D.text ">"]
        else D.join [D.text "<", impArgs', D.text ">"]

expPiArgsToDoc :: [RawBinder RawTerm] -> D.Doc
expPiArgsToDoc expArgs = do
  let expArgs' = piArgsToDoc expArgs
  if isMultiLine [expArgs']
    then D.join [D.text "(", D.line, D.nest D.indent expArgs', D.line, D.text ")"]
    else D.join [D.text "(", expArgs', D.text ")"]

expPiIntroArgsToDoc :: [RawBinder RawTerm] -> D.Doc
expPiIntroArgsToDoc expArgs = do
  let expArgs' = piIntroArgsToDoc expArgs
  if isMultiLine [expArgs']
    then D.join [D.text "(", D.line, D.nest D.indent expArgs', D.line, D.text ")"]
    else D.join [D.text "(", expArgs', D.text ")"]

clauseBodyToDoc :: RawTerm -> D.Doc
clauseBodyToDoc body = do
  let body' = toDoc body
  if isMultiLine [body']
    then D.join [D.text "{", D.nest D.indent $ D.join [D.line, body'], D.line, D.text "}"]
    else body'

recBody :: RawTerm -> D.Doc
recBody body = do
  let body' = toDoc body
  if isMultiLine [body']
    then D.join [D.text "{", D.nest D.indent $ D.join [D.line, body'], D.line, D.text "}"]
    else D.join [D.text "{", body', D.text "}"]

nameToDoc :: N.Name -> D.Doc
nameToDoc varOrLocator =
  case varOrLocator of
    N.Var var ->
      if isHole var
        then D.text "_"
        else D.text var
    N.Locator locator ->
      D.text $ Locator.reify locator

commaSeqH :: [D.Doc] -> D.Doc
commaSeqH docList =
  case docList of
    [] ->
      D.Nil
    [doc] ->
      doc
    doc : rest ->
      D.join [doc, D.text ", ", commaSeqH rest]

commaSeqV :: [D.Doc] -> D.Doc
commaSeqV docList =
  case docList of
    [] ->
      D.Nil
    [doc] ->
      doc
    doc : rest ->
      D.join [doc, D.text ",", D.line, commaSeqV rest]

listSeq :: [D.Doc] -> D.Doc
listSeq docList =
  case docList of
    [] ->
      D.Nil
    [doc] ->
      D.join [D.text "- ", D.nest D.indent doc]
    doc : rest ->
      D.join [D.text "- ", D.nest D.indent doc, D.line, listSeq rest]

isMultiLine :: [D.Doc] -> Bool
isMultiLine docList =
  case docList of
    [] ->
      False
    doc : rest ->
      case doc of
        D.Nil ->
          isMultiLine rest
        D.Text _ next ->
          isMultiLine $ next : rest
        D.Line {} ->
          True

piElimToDoc :: D.Doc -> [D.Doc] -> D.Doc
piElimToDoc e args = do
  if isMultiLine $ e : args
    then D.join [e, D.text "(", D.nest D.indent $ D.join [D.line, commaSeqV args], D.line, D.text ")"]
    else D.join [e, D.text "(", commaSeqH args, D.text ")"]

piElimKeyToDoc :: N.Name -> [(T.Text, RawTerm)] -> D.Doc
piElimKeyToDoc name kvs = do
  case getHorizontalDocList kvs of
    Just vs' ->
      D.join [nameToDoc name, D.text " of {", commaSeqH vs', D.text "}"]
    Nothing -> do
      let kvs' = map kvToDoc kvs
      D.join [nameToDoc name, D.text " of {", D.line, listSeq kvs', D.line, D.text "}"]

getHorizontalDocList :: [(T.Text, RawTerm)] -> Maybe [D.Doc]
getHorizontalDocList kvs =
  case kvs of
    [] ->
      Just []
    (k, v) : rest ->
      case v of
        _ :< Var (N.Var name)
          | k == name -> do
              rest' <- getHorizontalDocList rest
              return $ D.text name : rest'
        _ ->
          Nothing

kvToDoc :: (T.Text, RawTerm) -> D.Doc
kvToDoc (name, t) = do
  let t' = toDoc t
  if isMultiLine [t']
    then D.join [D.text name, D.text " = ", D.line, t']
    else D.join [D.text name, D.text " = ", t']

lowTypeToDoc :: LT.LowType -> D.Doc
lowTypeToDoc lt =
  case lt of
    LT.PrimNum primType ->
      primTypeToDoc primType
    LT.Pointer ->
      D.text "pointer"
    LT.Array {} -> do
      D.text "<array>"
    LT.Struct {} ->
      D.text "<struct>"
    LT.Function {} ->
      D.text "<function>"
    LT.Void ->
      D.text "void"
    LT.VarArgs ->
      D.text ".." -- unreachable

primTypeToDoc :: PT.PrimType -> D.Doc
primTypeToDoc primType =
  case primType of
    PT.Int intSize ->
      D.join [D.text "int", D.text (T.pack (show (PNS.intSizeToInt intSize)))]
    PT.Float floatSize ->
      D.join [D.text "int", D.text (T.pack (show (PNS.floatSizeToInt floatSize)))]

decodePrimOp :: P.PrimOp -> D.Doc
decodePrimOp op =
  case op of
    P.PrimUnaryOp op' dom _ ->
      D.join [D.text $ UnaryOp.getRep op' <> "-", primTypeToDoc dom]
    P.PrimBinaryOp op' dom _ ->
      D.join [D.text $ BinaryOp.getRep op' <> "-", primTypeToDoc dom]
    P.PrimCmpOp op' dom _ ->
      D.join [D.text $ CmpOp.getRep op' <> "-", primTypeToDoc dom]
    P.PrimConvOp op' dom cod -> do
      D.join [D.text $ T.pack (show op') <> "-", primTypeToDoc dom, D.text "-", primTypeToDoc cod]

decodeIntrospectClause :: (Maybe T.Text, RawTerm) -> D.Doc
decodeIntrospectClause (mKey, body) = do
  case mKey of
    Just key -> do
      D.join [D.text key, D.text " => ", D.line, toDoc body]
    Nothing ->
      D.join [D.text "default => ", D.line, toDoc body]

decodePatternRowList :: [RP.RawPatternRow RawTerm] -> [D.Doc]
decodePatternRowList =
  map decodePatternRow

decodePatternRow :: RP.RawPatternRow RawTerm -> D.Doc
decodePatternRow (patArgs, body) = do
  let patArgs' = map (decodePattern . snd) $ V.toList patArgs
  let body' = toDoc body
  D.join [commaSeqH patArgs', D.text " =>", D.line, body']

decodePattern :: RP.RawPattern -> D.Doc
decodePattern pat = do
  case pat of
    RP.Var name ->
      nameToDoc name
    RP.Cons name args -> do
      let name' = nameToDoc name
      case args of
        RP.Paren patList -> do
          let patList' = map (decodePattern . snd) patList
          D.join [name', D.text "(", commaSeqH patList', D.text ")"]
        RP.Of kvs -> do
          case getHorizontalDocList' (map (\(k, (_, v)) -> (k, v)) kvs) of
            Just vs' ->
              D.join [name', D.text " of {", commaSeqH vs', D.text "}"]
            Nothing -> do
              let kvs' = map (\(k, (_, v)) -> (k, v)) kvs
              let kvs'' = map kvToDoc' kvs'
              D.join [name', D.text " of {", D.line, listSeq kvs'', D.line, D.text "}"]
    RP.ListIntro patList -> do
      let patList' = map (decodePattern . snd) patList
      D.join [D.text "[", commaSeqH patList', D.text "]"]

getHorizontalDocList' :: [(T.Text, RP.RawPattern)] -> Maybe [D.Doc]
getHorizontalDocList' kvs =
  case kvs of
    [] ->
      Just []
    (k, v) : rest ->
      case v of
        RP.Var (N.Var name)
          | k == name -> do
              rest' <- getHorizontalDocList' rest
              return $ D.text name : rest'
        _ ->
          Nothing

kvToDoc' :: (T.Text, RP.RawPattern) -> D.Doc
kvToDoc' (name, v) = do
  let v' = decodePattern v
  if isMultiLine [v']
    then D.join [D.text name, D.text " =", D.nest D.indent $ D.join [D.line, v']]
    else D.join [D.text name, D.text " = ", v']

f :: RawBinder (a, C) -> RawBinder a
f (m, x, c1, c2, (t, _)) =
  (m, x, c1, c2, t)
