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
  case (container series, null (elems series)) of
    (Nothing, True) ->
      D.Nil
    (Nothing, _) ->
      PI.arrange
        [ PI.inject prefix',
          PI.inject $ PI.arrange $ intercalate (elems series) (hasTrailingComma series) (trailingComment series)
        ]
    (Just Angle, True) ->
      D.Nil
    (Just k, _) -> do
      let (open, close) = getContainerPair k
      let layout = if hasTrailingComma series then PI.nest else PI.idOrNest
      let arranger = if hasTrailingComma series then PI.arrangeVertical else PI.arrange
      PI.arrange
        [ PI.inject prefix',
          PI.inject $ D.text open,
          layout $ arranger $ intercalate (elems series) (hasTrailingComma series) (trailingComment series),
          PI.inject $ D.text close
        ]

intercalate :: [(C, D.Doc)] -> Bool -> C -> [PI.Piece]
intercalate elems hasTrailingComma trailingComment = do
  let elems' = map (uncurry attachComment) elems
  commaSeq elems' hasTrailingComma trailingComment

commaSeq :: [D.Doc] -> Bool -> C -> [PI.Piece]
commaSeq elems hasTrailingComma trailingComment =
  case elems of
    [] ->
      [PI.inject $ C.decode trailingComment]
    [d] -> do
      if hasTrailingComma
        then
          [ PI.inject d,
            PI.inject $ D.text ",",
            PI.inject $ C.asSuffix trailingComment
          ]
        else [PI.appendCommaIfVertical d, PI.inject $ C.asSuffix trailingComment]
    d : rest -> do
      [PI.inject d, PI.delimiterLeftAligned $ D.text ","] ++ commaSeq rest hasTrailingComma trailingComment

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

decodeHorizontallyIfPossible :: Series D.Doc -> D.Doc
decodeHorizontallyIfPossible series = do
  case container series of
    Just k | isHorizontalSeries series -> do
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
  null (trailingComment series) && isHorizontalSeries' (elems series) && not (hasTrailingComma series)

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
