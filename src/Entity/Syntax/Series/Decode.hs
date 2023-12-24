module Entity.Syntax.Series.Decode (decode) where

import Entity.C
import Entity.C.Decode qualified as C
import Entity.Doc qualified as D
import Entity.Syntax.Series

decode :: Series D.Doc -> D.Doc
decode series = do
  let prefix' = decodePrefix series
  case (container series, null (elems series)) of
    (Nothing, True) ->
      D.Nil
    (Nothing, _) ->
      D.join
        [ prefix',
          intercalate (separator series) (elems series) (trailingComment series)
        ]
    (Just Angle, True) ->
      D.Nil
    (Just k, _) -> do
      let (open, close) = getContainerPair k
      D.join
        [ prefix',
          D.text open,
          intercalate (separator series) (elems series) (trailingComment series),
          D.text close
        ]

intercalate :: Separator -> [(C, D.Doc)] -> C -> D.Doc
intercalate sep elems trailingComment = do
  let trailingComment' = C.decode trailingComment
  let elems' = map (uncurry attachComment) elems
  case sep of
    Comma -> do
      if D.isMulti (elems' ++ [trailingComment'])
        then D.join [D.nest D.indent $ D.join [D.line, commaSeqV elems' trailingComment], D.line]
        else D.commaSeqH $ map snd elems
    Hyphen ->
      D.join [listSeq elems trailingComment, D.line]

commaSeqV :: [D.Doc] -> C -> D.Doc
commaSeqV elems trail =
  case elems of
    [] ->
      C.decode trail
    [d] -> do
      D.join [d, C.asSuffix trail]
    d : rest -> do
      D.join [d, D.text ",", D.line, commaSeqV rest trail]

listSeq :: [(C, D.Doc)] -> C -> D.Doc
listSeq elems trail =
  case elems of
    [] ->
      D.nest D.indent $ C.decode trail
    [(c, d)] -> do
      if null trail
        then D.join [decodeListItem (c, d)]
        else D.join [decodeListItem (c, d), D.nest D.indent $ D.join [D.line, C.decode trail]]
    (c, d) : rest -> do
      D.join [decodeListItem (c, d), listSeq rest trail]

decodePrefix :: Series a -> D.Doc
decodePrefix series =
  case prefix series of
    Nothing ->
      D.Nil
    Just (p, c) -> do
      let p' = D.text $ " " <> p <> " "
      if null c
        then p'
        else
          D.join
            [ p',
              D.line,
              C.decode c,
              D.line
            ]

decodeListItem :: (C, D.Doc) -> D.Doc
decodeListItem (c, d) = do
  if null c
    then D.join [D.line, D.text "- ", D.nest D.indent d]
    else
      D.join
        [ D.nest D.indent $ D.join [D.line, C.decode c],
          D.line,
          D.join [D.text "- ", D.nest D.indent d]
        ]

attachComment :: C -> D.Doc -> D.Doc
attachComment c doc =
  D.join [C.asPrefix c, doc]
