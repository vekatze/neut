{- HLINT ignore "Use list comprehension" -}

module Language.RawTerm.RawTerm.ToDoc
  ( toDoc,
    typeToDoc,
    nameToDoc,
    typeAnnot,
    decodeArgs',
    decodeArgsMaybe,
    decodeDef,
    decodeTypeDef,
    decGeist,
    decTypeGeist,
    attachComment,
  )
where

import Control.Comonad.Cofree
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.Opacity qualified as O
import Language.Common.Rune qualified as RU
import Language.RawTerm.Key
import Language.RawTerm.Locator qualified as Locator
import Language.RawTerm.Name qualified as N
import Language.RawTerm.NecessityVariant (showNecessityVariant)
import Language.RawTerm.RawBinder
import Language.RawTerm.RawIdent
import Language.RawTerm.RawPattern qualified as RP
import Language.RawTerm.RawTerm
import Language.RawTerm.RawTerm qualified as RT
import Logger.Hint
import PrettyPrinter.Doc qualified as D
import PrettyPrinter.Piece qualified as PI
import SyntaxTree.C
import SyntaxTree.C.ToDoc qualified as C
import SyntaxTree.Series qualified as SE
import SyntaxTree.Series.ToDoc qualified as SE

toDoc :: RawTerm -> D.Doc
toDoc term =
  case term of
    _ :< Var varOrLocator ->
      nameToDoc varOrLocator
    _ :< VarGlobal dd _ ->
      D.text $ DD.reify dd -- unreachable
    _ :< PiIntro c def -> do
      decodeDef lambdaNameToDoc "function" c def
    _ :< PiIntroFix opacity c def -> do
      let keyword =
            case opacity of
              O.Opaque ->
                "define"
              O.Clear ->
                "inline"
      decodeDef (nameToDoc . N.Var) keyword c def
    _ :< PiElim e c1 mImpArgs c2 expArgs c3 mDefaultArgs -> do
      let expArgsDoc c =
            attachComment c $ SE.decodeHorizontallyIfPossible $ fmap toDoc expArgs
      case (mImpArgs, mDefaultArgs) of
        (Nothing, Nothing) ->
          PI.arrange
            [ PI.inject $ toDoc e,
              PI.inject $ expArgsDoc (c1 ++ c3)
            ]
        (Just impArgs, Nothing) ->
          PI.arrange
            [ PI.inject $ toDoc e,
              PI.inject $ attachComment c1 $ SE.decodeHorizontallyIfPossible $ fmap typeToDoc impArgs,
              PI.inject $ expArgsDoc (c2 ++ c3)
            ]
        (Nothing, Just defaultArgs) ->
          PI.arrange
            [ PI.inject $ toDoc e,
              PI.inject $ expArgsDoc c1,
              PI.inject $ attachComment c3 $ decPiElimKey defaultArgs
            ]
        (Just impArgs, Just defaultArgs) ->
          PI.arrange
            [ PI.inject $ toDoc e,
              PI.inject $ attachComment c1 $ SE.decodeHorizontallyIfPossible $ fmap typeToDoc impArgs,
              PI.inject $ expArgsDoc c2,
              PI.inject $ attachComment c3 $ decPiElimKey defaultArgs
            ]
    _ :< PiElimByKey name c kvs -> do
      PI.arrange
        [ PI.inject $ nameToDoc name,
          PI.inject $ attachComment c $ decPiElimKey kvs
        ]
    _ :< PiElimRule name c es -> do
      PI.arrange
        [ PI.inject $ attachComment c $ nameToDoc name,
          PI.inject $ SE.decodeHorizontallyIfPossible $ fmap toDoc es
        ]
    _ :< PiElimMeta name c es -> do
      PI.arrange
        [ PI.inject $ attachComment c $ nameToDoc name,
          PI.inject $ SE.decodeHorizontallyIfPossible $ fmap toDoc es
        ]
    _ :< PiElimExact c e ->
      PI.arrange
        [ PI.delimiterLeftAligned $ D.text "exact",
          PI.inject $ attachComment c $ toDoc e
        ]
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
    m :< BoxIntro c1 c2 vs (e, c3) -> do
      PI.arrange $
        [PI.horizontal $ attachComment c1 $ D.text "box"]
          ++ decodeQuoteVarList vs
          ++ [PI.inject $ toDoc $ m :< Brace c2 (e, c3)]
    m :< BoxIntroLift c1 c2 (e, c3) -> do
      PI.arrange
        [ PI.horizontal $ attachComment c1 $ D.text "lift",
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
    m :< CodeIntro codeVariant c1 c2 (e, c3) -> do
      PI.arrange
        [ PI.horizontal $ attachComment c1 $ D.text (codeVariantToKeyword codeVariant),
          PI.inject $ toDoc $ m :< Brace c2 (e, c3)
        ]
    m :< CodeElim c1 c2 (e, c3) -> do
      PI.arrange
        [ PI.horizontal $ attachComment c1 $ D.text "unquote",
          PI.inject $ toDoc $ m :< Brace c2 (e, c3)
        ]
    _ :< TauIntro c1 (c2, (ty, c3)) -> do
      PI.arrange
        [ PI.horizontal $ attachComment c1 $ D.text "pack-type",
          PI.inject $ decodeBrace' False c2 (typeToDoc ty) c3
        ]
    _ :< TauElim c1 (_, x, c2) c3 e1 c4 _ c5 e2 _ -> do
      D.join
        [ PI.arrange
            [ PI.beforeBareSeries $ D.text "unpack-type",
              PI.bareSeries $ attachComment (c1 ++ c2) $ D.text x
            ],
          PI.arrange
            [ PI.beforeBareSeries $ attachComment [] $ D.text "=",
              decodeLetBody c3 e1 c4
            ],
          D.line,
          attachComment c5 $ toDoc e2
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
    _ :< LetOn letKind c1 mxt c2 noeticVarList c3 e c4 _ c5 cont _ -> do
      D.join
        [ PI.arrange $
            [ PI.beforeBareSeries $ D.text $ RT.decodeLetKind letKind,
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
    _ :< RuneIntro _ r -> do
      D.text $ "`" <> T.replace "`" "\\`" (RU.asText r) <> "`"
    _ :< Magic c magic -> do
      case magic of
        Cast c1 from to e mc -> do
          let args =
                attachOptionalComment
                  mc
                  ( SE.fromListWithComment
                      (Just SE.Paren)
                      SE.Comma
                      [ RT.mapEL typeToDoc from,
                        RT.mapEL typeToDoc to,
                        RT.mapEL toDoc e
                      ]
                  )
          D.join
            [ attachComment (c ++ c1) $ D.text "magic cast",
              SE.decode args
            ]
        Store c1 t value pointer mc -> do
          D.join
            [ attachComment (c ++ c1) $ D.text "magic store",
              SE.decode $
                attachOptionalComment mc $
                  SE.fromListWithComment
                    (Just SE.Paren)
                    SE.Comma
                    [ RT.mapEL typeToDoc t,
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
                    [ RT.mapEL typeToDoc t,
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
                    [ RT.mapEL typeToDoc t,
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
                      RT.mapEL typeToDoc t
                    ]
            ]
        OpaqueValue c1 (c2, (e, c3)) -> do
          D.join
            [ attachComment (c ++ c1) $ D.text "magic opaque-value ",
              decodeBrace True c2 e c3
            ]
        CallType c1 func arg1 arg2 -> do
          D.join
            [ attachComment (c ++ c1) $ D.text "magic call-type",
              SE.decode $
                SE.fromListWithComment
                  (Just SE.Paren)
                  SE.Comma
                  [ RT.mapEL toDoc func,
                    RT.mapEL toDoc arg1,
                    RT.mapEL toDoc arg2
                  ]
            ]
        GetTypeTag (c1, (e, c2)) -> do
          D.join
            [ attachComment c $ D.text "magic get-type-tag",
              SE.decode $ SE.fromListWithComment (Just SE.Paren) SE.Comma [(c1, (typeToDoc e, c2))]
            ]
        ShowType c1 (c2, (typeExpr, c3)) -> do
          D.join
            [ attachComment (c ++ c1) $ D.text "magic show-type",
              SE.decode $ SE.fromListWithComment (Just SE.Paren) SE.Comma [(c2, (typeToDoc typeExpr, c3))]
            ]
        TextCons c1 (c2, (rune, c3)) (c4, (text, c5)) -> do
          D.join
            [ attachComment (c ++ c1) $ D.text "magic text-cons",
              SE.decode $
                SE.fromListWithComment
                  (Just SE.Paren)
                  SE.Comma
                  [ (c2, (toDoc rune, c3)),
                    (c4, (toDoc text, c5))
                  ]
            ]
        TextUncons c1 (c2, (text, c3)) -> do
          D.join
            [ attachComment (c ++ c1) $ D.text "magic text-uncons",
              SE.decode $ SE.fromListWithComment (Just SE.Paren) SE.Comma [(c2, (toDoc text, c3))]
            ]
        CompileError c1 (c2, (msg, c3)) -> do
          D.join
            [ attachComment (c ++ c1) $ D.text "magic compile-error",
              SE.decode $ SE.fromListWithComment (Just SE.Paren) SE.Comma [(c2, (toDoc msg, c3))]
            ]
    _ :< Annotation {} -> do
      D.text "<annot>"
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
    _ :< Int i ->
      D.text $ T.pack (show i)

typeToDoc :: RawType -> D.Doc
typeToDoc ty =
  case ty of
    _ :< Tau ->
      D.text "type"
    _ :< TypeHole {} ->
      D.text "_"
    _ :< TyVar name ->
      nameToDoc name
    _ :< TyApp t c args ->
      PI.arrange
        [ PI.inject $ typeToDoc t,
          PI.inject $ attachComment c $ SE.decodeHorizontallyIfPossible $ fmap typeToDoc args
        ]
    _ :< Pi (impArgs, c1) (expArgs, c3) (defaultArgs, c2) c cod _ -> do
      let hasDefault = not (SE.isEmpty defaultArgs)
      let expParamsBase = SE.decode $ fmap piArgToDoc expArgs
      let defaultParamsBase = decodeDefaultParams defaultArgs
      let expParamsWithImp = attachComment c1 expParamsBase
      let defaultParamsWithExpComment =
            if hasDefault
              then attachComment c3 defaultParamsBase
              else defaultParamsBase
      let cArrow =
            (if hasDefault then [] else c3) ++ c2
      PI.arrange
        [ PI.container $ decodeImpParams impArgs,
          PI.container expParamsWithImp,
          PI.container defaultParamsWithExpComment,
          PI.delimiter $ attachComment cArrow $ D.text "->",
          PI.inject $ attachComment c $ typeToDoc cod
        ]
    _ :< Data (AttrD.Attr {isConstLike}) dataName es -> do
      let base = D.text $ DD.reify dataName
      if isConstLike
        then base
        else
          PI.arrange
            [ PI.inject base,
              PI.inject $ SE.decodeHorizontallyIfPossible $ fmap typeToDoc $ SE.fromList' es
            ]
    _ :< Box t -> do
      D.join [D.text "+", typeToDoc t]
    _ :< BoxNoema t ->
      D.join [D.text "&", typeToDoc t]
    _ :< Code t -> do
      D.join [D.text "'", typeToDoc t]
    _ :< Rune ->
      D.text "rune"
    _ :< Pointer ->
      D.text "pointer"
    _ :< Void ->
      D.text "void"
    _ :< Option t ->
      D.join [D.text "?", typeToDoc t]
    _ :< TyBrace c1 (t, c2) -> do
      decodeBrace' False c1 (typeToDoc t) c2
    _ :< TyIntrospect c1 key c2 clauseList -> do
      PI.arrange
        [ PI.horizontal $ attachComment (c1 ++ c2) $ D.text "introspect",
          PI.horizontal $ D.text key,
          PI.inject $ SE.decode' $ fmap decodeTypeIntrospectClause clauseList
        ]

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

decodeTypeDef :: (a -> D.Doc) -> T.Text -> C -> RT.RawTypeDef a -> D.Doc
decodeTypeDef nameDecoder keyword c def = do
  attachComment c $
    D.join
      [ D.text keyword,
        D.text " ",
        decTypeGeist nameDecoder $ RT.typeGeist def,
        D.text " ",
        decodeBlock (RT.typeLeadingComment def, (typeToDoc $ RT.typeBody def, RT.typeTrailingComment def))
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

decodeArgs :: Args RawType -> D.Doc
decodeArgs (series, c) = do
  if null c
    then decodeBinder series
    else
      D.join
        [ decodeBinder series,
          C.decode c,
          D.line
        ]

decodeArgsMaybe :: Maybe (SE.Series (RawBinder RawType)) -> D.Doc
decodeArgsMaybe mArgs = do
  case mArgs of
    Nothing ->
      D.Nil
    Just args ->
      decodeArgs (args, [])

decodeArgs' :: Args RawType -> D.Doc
decodeArgs' (series, c) = do
  if null c
    then decodeBinder' series
    else
      D.join
        [ C.decode c,
          decodeBinder' series
        ]

decodeBinder :: SE.Series (RawBinder RawType) -> D.Doc
decodeBinder series =
  SE.decode $ fmap piArgToDoc series

decodeBinder' :: SE.Series (RawBinder RawType) -> D.Doc
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

piArgToDoc :: RawBinder RawType -> D.Doc
piArgToDoc (m, x, c1, c2, t) = do
  let t' = typeToDoc t
  if isHole x
    then attachComment (c1 ++ c2) t'
    else do
      let x' = D.text x
      paramToDoc' (m, x', c1, c2, t')

piIntroArgToDoc :: RawBinder RawType -> D.Doc
piIntroArgToDoc (m, x, c1, c2, t) = do
  let x' = nameToDoc $ N.Var x
  paramToDoc (m, x', c1, c2, t)

piIntroArgWithDefaultToDoc :: (RawBinder RawType, RawTerm) -> D.Doc
piIntroArgWithDefaultToDoc ((m, x, c1, c2, t), defaultValue) = do
  let x' = nameToDoc $ N.Var x
  let baseParam = paramToDoc (m, x', c1, c2, t)
  D.join [baseParam, D.text " := ", toDoc defaultValue]

varArgToDoc :: VarArg -> D.Doc
varArgToDoc (m, e, c1, c2, t) = do
  let e' = toDoc e
  let t' = typeToDoc t
  paramToDoc' (m, e', c1, c2, t')

paramToDoc :: (a, D.Doc, C, C, RawType) -> D.Doc
paramToDoc (m, x, c1, c2, t) = do
  case t of
    _ :< TypeHole {} ->
      attachComment (c1 ++ c2) x
    _ -> do
      paramToDoc' (m, x, c1, c2, typeToDoc t)

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
        defaultArgs = (defaultArgs, c2),
        expArgs = (expArgs, c3),
        cod = (c4, cod),
        isConstLike
      }
    ) = do
    let hasExp = (not isConstLike) || (not (SE.isEmpty expArgs))
    let hasDefault = not (SE.isEmpty defaultArgs)
    let expParamsBase = decodeExpParams isConstLike expArgs
    let defaultParamsBase = decodeDefaultParams defaultArgs
    let expParamsWithImp =
          if hasExp
            then attachComment c1 expParamsBase
            else expParamsBase
    let defaultParamsWithImp =
          if (not hasExp) && hasDefault
            then attachComment c1 defaultParamsBase
            else defaultParamsBase
    case cod of
      _ :< RT.TypeHole {} -> do
        let defaultParamsWithTrailing =
              if hasDefault
                then attachComment (c3 ++ c2) defaultParamsWithImp
                else defaultParamsWithImp
        let expParamsWithTrailing =
              if hasDefault
                then expParamsWithImp
                else attachComment (c3 ++ c2) expParamsWithImp
        PI.arrange
          [ PI.inject $ attachComment c0 $ nameDecoder name,
            PI.inject $ decodeImpParams impArgs,
            PI.inject expParamsWithTrailing,
            PI.inject defaultParamsWithTrailing
          ]
      _ -> do
        let defaultParamsWithExpComment =
              if hasDefault
                then attachComment c3 defaultParamsWithImp
                else defaultParamsWithImp
        let cColon =
              (if hasDefault then [] else c3) ++ c2
        let cColon' =
              if (not hasExp) && (not hasDefault)
                then c1 ++ cColon
                else cColon
        PI.arrange
          [ PI.inject $ attachComment c0 $ nameDecoder name,
            PI.inject $ decodeImpParams impArgs,
            PI.inject expParamsWithImp,
            PI.inject defaultParamsWithExpComment,
            PI.horizontal $ attachComment cColon' $ D.text ":",
            PI.inject $ attachComment c4 $ typeToDoc cod
          ]

decTypeGeist :: (a -> D.Doc) -> RT.RawGeist a -> D.Doc
decTypeGeist
  nameDecoder
  ( RT.RawGeist
      { name = (name, c0),
        impArgs = (impArgs, c1),
        defaultArgs = (defaultArgs, c2),
        expArgs = (expArgs, c3),
        isConstLike
      }
    ) = do
    let hasExp = (not isConstLike) || (not (SE.isEmpty expArgs))
    let hasDefault = not (SE.isEmpty defaultArgs)
    let expParamsBase = decodeExpParams isConstLike expArgs
    let defaultParamsBase = decodeDefaultParams defaultArgs
    let expParamsWithImp =
          if hasExp
            then attachComment c1 expParamsBase
            else expParamsBase
    let defaultParamsWithImp =
          if (not hasExp) && hasDefault
            then attachComment c1 defaultParamsBase
            else defaultParamsBase
    let defaultParamsWithTrailing =
          if hasDefault
            then attachComment (c3 ++ c2) defaultParamsWithImp
            else defaultParamsWithImp
    let expParamsWithTrailing =
          if hasDefault
            then expParamsWithImp
            else attachComment (c3 ++ c2) expParamsWithImp
    PI.arrange
      [ PI.inject $ attachComment c0 $ nameDecoder name,
        PI.inject $ decodeImpParams impArgs,
        PI.inject expParamsWithTrailing,
        PI.inject defaultParamsWithTrailing
      ]

decodeImpParams :: SE.Series (RawBinder RawType) -> D.Doc
decodeImpParams impParams =
  if SE.isEmpty impParams
    then D.Nil
    else SE.decode $ fmap decodeImpVar impParams

decodeImpVar :: RawBinder RawType -> D.Doc
decodeImpVar (m, x, c1, c2, t) = do
  if isHole x
    then attachComment (c1 ++ c2) (typeToDoc t)
    else paramToDoc (m, D.text x, c1, c2, t)

decodeDefaultParams :: SE.Series (RawBinder RawType, RawTerm) -> D.Doc
decodeDefaultParams defaultParams =
  if SE.isEmpty defaultParams
    then D.Nil
    else SE.decode $ fmap piIntroArgWithDefaultToDoc defaultParams

decodeExpParams :: Bool -> SE.Series (RawBinder RawType) -> D.Doc
decodeExpParams isConstLike expParams =
  if isConstLike && SE.isEmpty expParams
    then D.Nil
    else SE.decode $ fmap piIntroArgToDoc expParams

letArgToDoc :: (a, RP.RawPattern, C, C, RawType) -> D.Doc
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
        D.InlineComment {} ->
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

decodeTypeIntrospectClause :: (Maybe T.Text, C, RawType) -> (D.Doc, T.Text, D.Doc)
decodeTypeIntrospectClause (mKey, c, body) = do
  let key = D.text $ fromMaybe "default" mKey
  (PI.arrange [PI.container key], "=>", attachComment c $ typeToDoc body)

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
