module Entity.Syntax.Series.Decode
  ( decode,
    decodeHorizontallyIfPossible,
  )
where

import Entity.C
import Entity.C.Decode qualified as C
import Entity.Doc qualified as D
import Entity.Piece qualified as PI
import Entity.Syntax.Series

decode :: Series D.Doc -> D.Doc
decode series = do
  let prefix' = decodePrefix series
  let sep = separator series
  case (container series, null (elems series)) of
    (Nothing, True) ->
      D.Nil
    (Nothing, _) ->
      PI.arrange
        [ PI.inject prefix',
          PI.inject $ PI.arrange $ intercalate sep (elems series) (trailingComment series)
        ]
    (Just Angle, True) ->
      D.Nil
    (Just k, _) -> do
      let (open, close) = getContainerPair k
      case sep of
        Hyphen ->
          PI.arrange
            [ PI.inject prefix',
              PI.inject $ D.text open,
              PI.inject $ PI.arrange $ intercalate sep (elems series) (trailingComment series),
              PI.inject $ D.text close
            ]
        Comma -> do
          PI.arrange
            [ PI.inject prefix',
              PI.inject $ D.text open,
              PI.idOrNest $ PI.arrange $ intercalate sep (elems series) (trailingComment series),
              PI.inject $ D.text close
            ]

intercalate :: Separator -> [(C, D.Doc)] -> C -> [PI.Piece]
intercalate sep elems trailingComment = do
  case sep of
    Comma -> do
      let elems' = map (uncurry attachComment) elems
      commaSeq elems' trailingComment
    Hyphen ->
      [PI.inject $ D.join [listSeq elems trailingComment, D.line]]

commaSeq :: [D.Doc] -> C -> [PI.Piece]
commaSeq elems trail =
  case elems of
    [] ->
      [PI.inject $ C.decode trail]
    [d] -> do
      [PI.inject d, PI.inject $ C.asSuffix trail]
    d : rest -> do
      [PI.inject d, PI.delimiterLeftAligned $ D.text ","] ++ commaSeq rest trail

listSeq :: [(C, D.Doc)] -> C -> D.Doc
listSeq elems trail =
  case elems of
    [] ->
      if null trail
        then D.Nil
        else D.nest D.indent $ D.join [D.line, C.decode trail]
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

decodeHorizontallyIfPossible :: Series D.Doc -> D.Doc
decodeHorizontallyIfPossible series = do
  case separator series of
    Comma
      | Just k <- container series,
        isHorizontalSeries series -> do
          let prefix' = decodePrefix series
          let (open, close) = getContainerPair k
          let elems' = map snd $ elems series
          PI.arrange
            [ PI.inject prefix',
              PI.inject $ D.text open,
              PI.inject $ PI.arrange $ commaSeqHorizontal elems',
              PI.inject $ D.text close
            ]
    _ ->
      decode series

isHorizontalSeries :: Series D.Doc -> Bool
isHorizontalSeries series = do
  null (trailingComment series) && isHorizontalSeries' (elems series)

-- [single, ..., single, multi, .., multi] <=> True
isHorizontalSeries' :: [(C, D.Doc)] -> Bool
isHorizontalSeries' elems =
  case elems of
    [] ->
      True
    (c, d) : rest -> do
      let isMulti = not (null c) || D.isMulti [d]
      if isMulti
        then isHorizontalSeries'' rest
        else isHorizontalSeries' rest

isHorizontalSeries'' :: [(C, D.Doc)] -> Bool
isHorizontalSeries'' elems =
  case elems of
    [] ->
      True
    (c, d) : rest -> do
      let isMulti = not (null c) || D.isMulti [d]
      isMulti && isHorizontalSeries'' rest

commaSeqHorizontal :: [D.Doc] -> [PI.Piece]
commaSeqHorizontal elems =
  case elems of
    [] ->
      []
    [d] -> do
      [PI.inject d]
    d : rest -> do
      [PI.inject d, PI.horizontal $ D.text ","] ++ commaSeqHorizontal rest

attachComment :: C -> D.Doc -> D.Doc
attachComment c doc =
  D.join [C.asPrefix c, doc]
