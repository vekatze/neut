{- HLINT ignore "Use list comprehension" -}

module Language.RawTerm.Rule.RawTerm.ToDoc
  ( pp,
    toDoc,
    nameToDoc,
    typeAnnot,
    decodeArgs,
    decodeArgs',
    decodeArgsMaybe,
    decodeDef,
    decGeist,
    decodeImpParams,
    decodeImpParamsMaybe,
    attachComment,
    decodeBlock,
    decodeKeywordClause,
  )
where

import Control.Comonad.Cofree
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.Rune qualified as RU
import Language.RawTerm.Rule.Key
import Language.RawTerm.Rule.Locator qualified as Locator
import Language.RawTerm.Rule.Name qualified as N
import Language.RawTerm.Rule.NecessityVariant (showNecessityVariant)
import Language.RawTerm.Rule.RawBinder
import Language.RawTerm.Rule.RawIdent
import Language.RawTerm.Rule.RawPattern qualified as RP
import Language.RawTerm.Rule.RawTerm
import Language.RawTerm.Rule.RawTerm qualified as RT
import Logger.Rule.Hint
import PrettyPrinter.Rule.Doc qualified as D
import PrettyPrinter.Rule.Piece qualified as PI
import SyntaxTree.Rule.C
import SyntaxTree.Rule.C.ToDoc qualified as C
import SyntaxTree.Rule.Series qualified as SE
import SyntaxTree.Rule.Series.ToDoc qualified as SE

pp :: RawTerm -> T.Text
pp e = do
  D.layout $ toDoc e

toDoc :: RawTerm -> D.Doc
toDoc term =
  case term of
    _ :< Tau ->
      D.text "type"
    _ :< Var varOrLocator ->
      nameToDoc varOrLocator
    _ :< Pi (impArgs, c1) (expArgs, c2) c cod _ -> do
      PI.arrange
        [ PI.container $ decodeImpParams impArgs,
          PI.container $ attachComment c1 $ SE.decode $ fmap piArgToDoc expArgs,
          PI.delimiter $ attachComment c2 $ D.text "->",
          PI.inject $ attachComment c $ toDoc cod
        ]
    _ :< PiIntro c def -> do
      decodeDef lambdaNameToDoc "function" c def
    _ :< PiIntroFix c def -> do
      decodeDef (nameToDoc . N.Var) "define" c def
    _ :< PiElim e c impArgs expArgs -> do
      PI.arrange
        [ PI.inject $ toDoc e,
          PI.inject $ decodeImpArgs impArgs,
          PI.inject $ attachComment c $ SE.decodeHorizontallyIfPossible $ fmap toDoc expArgs
        ]
    _ :< PiElimByKey name c kvs -> do
      PI.arrange
        [ PI.inject $ nameToDoc name,
          PI.inject $ attachComment c $ decPiElimKey kvs
        ]
    _ :< PiElimExact c e ->
      PI.arrange
        [ PI.delimiterLeftAligned $ D.text "exact",
          PI.inject $ attachComment c $ toDoc e
        ]
    _ :< Data _ dataName _ ->
      D.text $ DD.reify dataName
    _ :< DataIntro _ dataIntroName _ _ ->
      D.text $ DD.reify dataIntroName
    _ :< DataElim c isNoetic es patternRowList -> do
      D.join
        [ PI.arrange
            [ PI.beforeBareSeries $ if isNoetic then D.text "case" else D.text "match",
              PI.bareSeries $ attachComment c $ SE.decode $ fmap toDoc es
            ],
          SE.decode' $ fmap decodePatternRow patternRowList
        ]
    _ :< Box t -> do
      PI.arrange
        [ PI.horizontal $ D.text "meta",
          PI.inject $ toDoc t
        ]
    _ :< BoxNoema t ->
      D.join [D.text "&", toDoc t]
    m :< BoxIntro c1 c2 vs (e, c3) -> do
      PI.arrange $
        [PI.horizontal $ attachComment c1 $ D.text "box"]
          ++ decodeQuoteVarList vs
          ++ [PI.inject $ toDoc $ m :< Brace c2 (e, c3)]
    m :< BoxIntroQuote c1 c2 (e, c3) -> do
      PI.arrange
        [ PI.horizontal $ attachComment c1 $ D.text "quote",
          PI.inject $ toDoc $ m :< Brace c2 (e, c3)
        ]
    _ :< BoxElim nv _ c1 mxt c2 noeticVarList c3 e c4 _ c5 cont _ -> do
      let keyword = showNecessityVariant nv
      D.join
        [ PI.arrange $
            [ PI.beforeBareSeries $ D.text keyword,
              PI.bareSeries $ D.join [attachComment c1 $ letArgToDoc mxt, C.asSuffix c2]
            ]
              ++ decodeNoeticVarList noeticVarList,
          PI.arrange
            [ PI.beforeBareSeries $ D.text "=",
              decodeLetBody c3 e c4
            ],
          D.line,
          attachComment c5 $ toDoc cont
        ]
    _ :< Embody e ->
      D.join [D.text "*", toDoc e]
    _ :< Let letKind c1 mxt c2 c3 e c4 _ c5 cont _ -> do
      D.join
        [ PI.arrange
            [ PI.beforeBareSeries $ D.text $ RT.decodeLetKind letKind,
              PI.bareSeries $ D.join [attachComment c1 $ letArgToDoc mxt, C.asSuffix c2]
            ],
          PI.arrange
            [ PI.beforeBareSeries $ D.text "=",
              decodeLetBody c3 e c4
            ],
          D.line,
          attachComment c5 $ toDoc cont
        ]
    _ :< LetOn _ c1 mxt c2 noeticVarList c3 e c4 _ c5 cont _ -> do
      D.join
        [ PI.arrange $
            [ PI.beforeBareSeries $ D.text "let",
              PI.bareSeries $ D.join [attachComment c1 $ letArgToDoc mxt, C.asSuffix c2]
            ]
              ++ decodeNoeticVarList noeticVarList,
          PI.arrange
            [ PI.beforeBareSeries $ D.text "=",
              decodeLetBody c3 e c4
            ],
          D.line,
          attachComment c5 $ toDoc cont
        ]
    _ :< Pin c1 mxt c2 noeticVarList c3 e1 c4 _ c5 e2 _ -> do
      D.join
        [ PI.arrange $
            [ PI.beforeBareSeries $ D.text "pin",
              PI.bareSeries $ D.join [attachComment c1 $ piIntroArgToDoc mxt, C.asSuffix c2]
            ]
              ++ decodeNoeticVarList noeticVarList,
          PI.arrange
            [ PI.beforeBareSeries $ D.text "=",
              decodeLetBody c3 e1 c4
            ],
          D.line,
          attachComment c5 $ toDoc e2
        ]
    _ :< StaticText _ txt -> do
      D.text $ "\"" <> txt <> "\""
    _ :< Rune -> do
      D.text "rune"
    _ :< RuneIntro _ r -> do
      D.text $ "`" <> T.replace "`" "\\`" (RU.asText r) <> "`"
    _ :< Magic c magic -> do
      case magic of
        Cast c1 from to e mc -> do
          let args = attachOptionalComment mc $ SE.fromListWithComment (Just SE.Paren) SE.Comma [from, to, e]
          D.join
            [ attachComment (c ++ c1) $ D.text "magic cast",
              SE.decode $ toDoc <$> args
            ]
        Store c1 t value pointer mc -> do
          D.join
            [ attachComment (c ++ c1) $ D.text "magic store",
              SE.decode $
                attachOptionalComment mc $
                  SE.fromListWithComment
                    (Just SE.Paren)
                    SE.Comma
                    [ RT.mapEL toDoc t,
                      RT.mapEL toDoc value,
                      RT.mapEL toDoc pointer
                    ]
            ]
        Load c1 t pointer mc -> do
          D.join
            [ attachComment (c ++ c1) $ D.text "magic load",
              SE.decode $
                attachOptionalComment mc $
                  SE.fromListWithComment
                    (Just SE.Paren)
                    SE.Comma
                    [ RT.mapEL toDoc t,
                      RT.mapEL toDoc pointer
                    ]
            ]
        Alloca c1 t size mc -> do
          D.join
            [ attachComment (c ++ c1) $ D.text "magic alloca",
              SE.decode $
                attachOptionalComment mc $
                  SE.fromListWithComment
                    (Just SE.Paren)
                    SE.Comma
                    [ RT.mapEL toDoc t,
                      RT.mapEL toDoc size
                    ]
            ]
        External c1 _ funcName c2 args varArgsOrNone -> do
          let args' = SE.decode $ fmap toDoc args
          case varArgsOrNone of
            Nothing ->
              D.join
                [ attachComment (c ++ c1) $ D.text $ "magic external " <> EN.reify funcName,
                  attachComment c2 args'
                ]
            Just (c3, varArgs) -> do
              PI.arrange
                [ PI.inject $ attachComment (c ++ c1) $ D.text $ "magic external " <> EN.reify funcName,
                  PI.inject $ attachComment c2 args',
                  PI.inject $ attachComment c3 $ SE.decode $ fmap varArgToDoc varArgs
                ]
        Global c1 name t mc -> do
          D.join
            [ attachComment (c ++ c1) $ D.text "magic global",
              SE.decode $
                attachOptionalComment mc $
                  SE.fromListWithComment
                    (Just SE.Paren)
                    SE.Comma
                    [ RT.mapEL (D.text . T.pack . show . EN.reify) name,
                      RT.mapEL toDoc t
                    ]
            ]
        OpaqueValue c1 (c2, (e, c3)) -> do
          D.join
            [ attachComment (c ++ c1) $ D.text "magic opaque-value ",
              decodeBrace True c2 e c3
            ]
    _ :< Hole {} ->
      D.text "_"
    _ :< Annotation {} -> do
      D.text "<annot>"
    _ :< Resource dd _ _ _ -> do
      D.text $ DD.localLocator dd
    _ :< If ifClause elseIfClauseList elseBody -> do
      let ifClause' = decodeKeywordClause "if" $ mapKeywordClause toDoc ifClause
      let elseIfClauseList' = map (decodeKeywordClause "else-if" . mapKeywordClause toDoc) elseIfClauseList
      let elseBody' = decodeBlock $ RT.mapEL toDoc elseBody
      D.join
        [ PI.arrange
            [ PI.horizontal ifClause',
              PI.horizontal $ D.intercalate (D.text " ") elseIfClauseList'
            ],
          PI.arrange
            [ PI.horizontal $ D.text "else",
              PI.inject elseBody'
            ]
        ]
    _ :< When whenClause -> do
      decodeKeywordClause "when" $ mapKeywordClause toDoc whenClause
    _ :< Seq (e1, c1) c2 e2 -> do
      D.join [toDoc e1, D.text ";", D.line, attachComment (c1 ++ c2) $ toDoc e2]
    _ :< SeqEnd e1 -> do
      D.join [toDoc e1, D.text ";"]
    _ :< ListIntro es -> do
      SE.decode $ fmap toDoc es
    _ :< Admit ->
      D.text "admit"
    m :< Detach c1 c2 (e, c3) -> do
      PI.arrange
        [ PI.horizontal $ attachComment c1 $ D.text "detach",
          PI.inject $ toDoc $ m :< Brace c2 (e, c3)
        ]
    m :< Attach c1 c2 (e, c3) -> do
      PI.arrange
        [ PI.horizontal $ attachComment c1 $ D.text "attach",
          PI.inject $ toDoc $ m :< Brace c2 (e, c3)
        ]
    _ :< Option t -> do
      D.join [D.text "?", toDoc t]
    _ :< Assert c1 (_, message) c2 c3 (e, c4) -> do
      let message' = D.text (T.pack (show message))
      decodeKeywordClause "assert" ((c1, (message', c2)), (c3, (toDoc e, c4)))
    _ :< Introspect c1 key c2 clauseList -> do
      PI.arrange
        [ PI.horizontal $ attachComment (c1 ++ c2) $ D.text "introspect",
          PI.horizontal $ D.text key,
          PI.inject $ SE.decode' $ fmap decodeIntrospectClause clauseList
        ]
    _ :< IncludeText c1 c2 _ (path, c3) -> do
      let args = SE.fromListWithComment (Just SE.Paren) SE.Comma [(c2, (path, c3))]
      PI.arrange
        [ PI.inject $ D.text "include-text",
          PI.inject $ attachComment c1 $ SE.decodeHorizontallyIfPossible $ fmap D.text args
        ]
    _ :< Brace c1 (e, c2) -> do
      decodeBrace False c1 e c2
    _ :< Pointer ->
      D.text "pointer"
    _ :< Void ->
      D.text "void"

decodeDef :: (a -> D.Doc) -> T.Text -> C -> RawDef a -> D.Doc
decodeDef nameDecoder keyword c def = do
  attachComment c $
    D.join
      [ D.text keyword,
        D.text " ",
        decGeist nameDecoder $ RT.geist def,
        D.text " ",
        decodeBlock (RT.leadingComment def, (toDoc $ RT.body def, RT.trailingComment def))
      ]

decodeKeywordClause :: T.Text -> KeywordClause D.Doc -> D.Doc
decodeKeywordClause k ((c1, (cond, c2)), body) = do
  attachComment (c1 ++ c2) $
    D.join
      [ PI.arrange
          [ PI.beforeBareSeries $ D.text k,
            PI.bareSeries cond
          ],
        decodeBlock body
      ]

decodeLetBody :: C -> RawTerm -> C -> PI.Piece
decodeLetBody c1 e c2 = do
  case e of
    _ :< Brace c3 (e', c4) -> do
      let block = decodeBlock (c3, (toDoc e', c4))
      PI.inject $ D.join [D.text " ", attachComment c1 block, D.text ";", C.asSuffix c2]
    _ ->
      PI.letBody $ D.join [attachComment c1 $ toDoc e, D.text ";", C.asSuffix c2]

decodeBlock :: EL D.Doc -> D.Doc
decodeBlock (c1, (body, c2)) = do
  D.join
    [ D.text "{",
      D.nest D.indent $ D.join [D.line, attachComment c1 body, C.asSuffix c2],
      D.line,
      D.text "}"
    ]

decodeImpArgs :: Maybe (SE.Series RawTerm) -> D.Doc
decodeImpArgs mImpArgs =
  case mImpArgs of
    Nothing ->
      D.Nil
    Just series -> do
      SE.decodeHorizontallyIfPossible $ fmap toDoc series

decodeArgs :: Args RawTerm -> D.Doc
decodeArgs (series, c) = do
  if null c
    then decodeBinder series
    else
      D.join
        [ decodeBinder series,
          C.decode c,
          D.line
        ]

decodeArgsMaybe :: Maybe (SE.Series (RawBinder RawTerm)) -> D.Doc
decodeArgsMaybe mArgs = do
  case mArgs of
    Nothing ->
      D.Nil
    Just args ->
      decodeArgs (args, [])

decodeArgs' :: Args RawTerm -> D.Doc
decodeArgs' (series, c) = do
  if null c
    then decodeBinder' series
    else
      D.join
        [ C.decode c,
          decodeBinder' series
        ]

decodeBinder :: SE.Series (RawBinder RawTerm) -> D.Doc
decodeBinder series =
  SE.decode $ fmap piArgToDoc series

decodeBinder' :: SE.Series (RawBinder RawTerm) -> D.Doc
decodeBinder' series =
  SE.decode $ fmap piIntroArgToDoc series

decodeNoeticVar :: (Hint, RawIdent) -> D.Doc
decodeNoeticVar (_, v) =
  D.text v

decodeNoeticVarList :: SE.Series (Hint, RawIdent) -> [PI.Piece]
decodeNoeticVarList vs =
  if SE.isEmpty vs
    then []
    else
      [ PI.beforeBareSeries $ D.text "on",
        PI.bareSeries $ SE.decode $ fmap decodeNoeticVar vs
      ]

decodeQuoteVarList :: SE.Series (Hint, RawIdent) -> [PI.Piece]
decodeQuoteVarList vs =
  if SE.isEmpty vs
    then []
    else [PI.horizontal $ SE.decode $ fmap decodeNoeticVar vs]

piArgToDoc :: RawBinder RawTerm -> D.Doc
piArgToDoc (m, x, c1, c2, t) = do
  let t' = toDoc t
  if isHole x
    then attachComment (c1 ++ c2) t'
    else do
      let x' = D.text x
      paramToDoc' (m, x', c1, c2, t')

piIntroArgToDoc :: RawBinder RawTerm -> D.Doc
piIntroArgToDoc (m, x, c1, c2, t) = do
  let x' = nameToDoc $ N.Var x
  paramToDoc (m, x', c1, c2, t)

piIntroArgWithDefaultToDoc :: (RawBinder RawTerm, Maybe RawTerm) -> D.Doc
piIntroArgWithDefaultToDoc ((m, x, c1, c2, t), maybeDefault) = do
  let x' = nameToDoc $ N.Var x
  let baseParam = paramToDoc (m, x', c1, c2, t)
  case maybeDefault of
    Nothing -> baseParam
    Just defaultValue -> D.join [baseParam, D.text " := ", toDoc defaultValue]

varArgToDoc :: VarArg -> D.Doc
varArgToDoc (m, e, c1, c2, t) = do
  let e' = toDoc e
  let t' = toDoc t
  paramToDoc' (m, e', c1, c2, t')

paramToDoc :: (a, D.Doc, C, C, RawTerm) -> D.Doc
paramToDoc (m, x, c1, c2, t) = do
  case t of
    _ :< Hole {} ->
      attachComment (c1 ++ c2) x
    _ -> do
      paramToDoc' (m, x, c1, c2, toDoc t)

paramToDoc' :: (a, D.Doc, C, C, D.Doc) -> D.Doc
paramToDoc' (_, x, c1, c2, t) = do
  PI.arrange
    [ PI.parameter x,
      PI.inject $ attachComment (c1 ++ c2) $ typeAnnot t
    ]

decGeist :: (a -> D.Doc) -> RT.RawGeist a -> D.Doc
decGeist
  nameDecoder
  ( RT.RawGeist
      { name = (name, c0),
        impArgs = (impArgs, c1),
        expArgs = (expArgs, c2),
        cod = (c3, cod),
        isConstLike
      }
    ) =
    case cod of
      _ :< RT.Hole {} ->
        PI.arrange
          [ PI.inject $ attachComment c0 $ nameDecoder name,
            PI.inject $ decodeImpParams impArgs,
            PI.inject $ attachComment c1 $ decodeExpParams isConstLike expArgs
          ]
      _ ->
        PI.arrange
          [ PI.inject $ attachComment c0 $ nameDecoder name,
            PI.inject $ decodeImpParams impArgs,
            PI.inject $ attachComment c1 $ decodeExpParams isConstLike expArgs,
            PI.horizontal $ attachComment c2 $ D.text ":",
            PI.inject $ attachComment c3 $ toDoc cod
          ]

decodeImpParams :: SE.Series (RawBinder RawTerm, Maybe RawTerm) -> D.Doc
decodeImpParams impParams =
  if SE.isEmpty impParams
    then D.Nil
    else SE.decode $ fmap piIntroArgWithDefaultToDoc impParams

decodeImpParamsMaybe :: Maybe (SE.Series (RawBinder RawTerm, Maybe RawTerm)) -> D.Doc
decodeImpParamsMaybe =
  maybe D.Nil decodeImpParams

decodeExpParams :: Bool -> SE.Series (RawBinder RawTerm) -> D.Doc
decodeExpParams isConstLike expParams =
  if isConstLike && SE.isEmpty expParams
    then D.Nil
    else SE.decode $ fmap piIntroArgToDoc expParams

letArgToDoc :: (a, RP.RawPattern, C, C, RawTerm) -> D.Doc
letArgToDoc (m, x, c1, c2, t) = do
  let x' = decodePattern x
  paramToDoc (m, x', c1, c2, t)

typeAnnot :: D.Doc -> D.Doc
typeAnnot t = do
  if isMultiLine [t]
    then D.join [D.text ":", D.line, t]
    else D.join [D.text ": ", t]

nameToDoc :: N.Name -> D.Doc
nameToDoc varOrLocator =
  case varOrLocator of
    N.Var var ->
      if isHole var
        then D.text "_"
        else D.text var
    N.Locator locator ->
      D.text $ Locator.reify locator

lambdaNameToDoc :: Maybe T.Text -> D.Doc
lambdaNameToDoc =
  maybe D.Nil D.text

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

decPiElimKey :: SE.Series (Hint, Key, C, C, RawTerm) -> D.Doc
decPiElimKey kvs = do
  let kvs' = fmap decPiElimKeyItem kvs
  SE.decode $ fmap decPiElimKeyItem' kvs'

type Rhymed =
  Bool

decPiElimKeyItem :: (Hint, Key, C, C, RawTerm) -> (Key, C, Rhymed, RawTerm)
decPiElimKeyItem (_, k, c1, c2, e) =
  case e of
    _ :< Var (N.Var k')
      | k == k' ->
          (k, c1 ++ c2, True, e)
    _ ->
      (k, c1 ++ c2, False, e)

decPiElimKeyItem' :: (Key, C, Rhymed, RawTerm) -> D.Doc
decPiElimKeyItem' (k, c, b, d) = do
  if b
    then D.text k
    else
      PI.arrange
        [ PI.horizontal $ D.text k,
          PI.horizontal $ D.text ":=",
          decodeClauseBody c d
        ]

decodeIntrospectClause :: (Maybe T.Text, C, RawTerm) -> (D.Doc, T.Text, D.Doc)
decodeIntrospectClause (mKey, c, body) = do
  let key = D.text $ fromMaybe "default" mKey
  decodeDoubleArrowClause (key, c, body)

decodePatternRow :: RP.RawPatternRow RawTerm -> (D.Doc, T.Text, D.Doc)
decodePatternRow (patArgs, c, body) = do
  let patArgs' = SE.decode $ fmap (decodePattern . snd) patArgs
  decodeDoubleArrowClause (patArgs', c, body)

decodeDoubleArrowClause :: (D.Doc, C, RawTerm) -> (D.Doc, T.Text, D.Doc)
decodeDoubleArrowClause (dom, c, cod) = do
  (PI.arrange [PI.container dom], "=>", attachComment c $ toDoc cod)

decodeClauseBody :: C -> RawTerm -> PI.Piece
decodeClauseBody c e = do
  case e of
    _ :< RT.Brace c1 (inner, c2) -> do
      PI.inject $ attachComment c $ decodeBrace True c1 inner c2
    _ -> do
      let baseDoc = toDoc e
      if D.isMulti [baseDoc]
        then PI.inject $ decodeBrace' True c baseDoc []
        else PI.inject $ attachComment c $ toDoc e

decodePattern :: RP.RawPattern -> D.Doc
decodePattern pat = do
  case pat of
    RP.Var name ->
      nameToDoc name
    RP.Cons name c args -> do
      let name' = nameToDoc name
      case args of
        RP.Paren patList -> do
          let patList' = SE.decode $ fmap (decodePattern . snd) patList
          D.join [name', attachComment c patList']
        RP.Of kvs -> do
          let kvs' = SE.decode $ fmap decodePatternKeyValue kvs
          D.join [name', attachComment c kvs']
    RP.ListIntro patList -> do
      SE.decode $ fmap (decodePattern . snd) patList
    RP.RuneIntro r ->
      D.text $ "`" <> T.replace "`" "\\`" (RU.asText r) <> "`"

decodePatternKeyValue :: (Key, (Hint, C, RP.RawPattern)) -> D.Doc
decodePatternKeyValue (k, (_, c, v)) = do
  case v of
    RP.Var (N.Var k')
      | k == k' ->
          D.text k
    _ ->
      PI.arrange
        [ PI.inject $ D.text k,
          PI.clauseDelimiter $ D.text ":=",
          PI.inject $ attachComment c $ decodePattern v
        ]

attachComment :: C -> D.Doc -> D.Doc
attachComment c doc =
  D.join [C.asPrefix c, doc]

decodeBrace :: Bool -> C -> RawTerm -> C -> D.Doc
decodeBrace forceVertical c1 e c2 = do
  decodeBrace' forceVertical c1 (toDoc e) c2

decodeBrace' :: Bool -> C -> D.Doc -> C -> D.Doc
decodeBrace' forceVertical c1 d c2 = do
  let layout = if forceVertical then PI.nest else PI.idOrNest
  PI.arrange
    [ PI.inject $ D.text "{",
      layout $ D.join [attachComment c1 d, C.asSuffix c2],
      PI.inject $ D.text "}"
    ]

attachOptionalComment :: Maybe C -> SE.Series a -> SE.Series a
attachOptionalComment mc se =
  case mc of
    Nothing ->
      se
    Just c ->
      se
        { SE.trailingComment = SE.trailingComment se ++ c,
          SE.hasOptionalSeparator = True
        }
