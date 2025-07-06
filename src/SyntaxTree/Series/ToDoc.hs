module SyntaxTree.Series.ToDoc
  ( decode,
    decode',
    decodeHorizontallyIfPossible,
  )
where

import Data.Text qualified as T
import PrettyPrinter.Doc qualified as D
import PrettyPrinter.Piece qualified as PI
import SyntaxTree.C
import SyntaxTree.C.ToDoc qualified as C
import SyntaxTree.Series

decode :: Series D.Doc -> D.Doc
decode = do
  _decode False

_decode :: Bool -> Series D.Doc -> D.Doc
_decode forceVertical series = do
  let prefix' = decodePrefix series
  let sep = separator series
  case (container series, isEmpty series) of
    (Nothing, True) ->
      D.Nil
    (Nothing, _) -> do
      let isVertical = forceVertical || hasOptionalSeparator series || hasComment series
      let intercalate =
            case sep of
              Bar -> barSeq
              Comma -> commaSeq
      PI.arrange
        [ PI.inject prefix',
          PI.inject $ PI.arrange $ intercalate (elems series) isVertical (trailingComment series)
        ]
    (Just k, _) -> do
      let (open, close) = getContainerPair k
      let isVertical = forceVertical || hasOptionalSeparator series || hasComment series
      let arranger = if isVertical then PI.arrangeVertical else PI.arrange
      case sep of
        Bar -> do
          let layout = if isVertical then PI.block else PI.idOrNest
          PI.arrange
            [ PI.inject prefix',
              PI.inject $ D.text open,
              layout $ arranger $ barSeq (elems series) isVertical (trailingComment series),
              PI.inject $ D.text close
            ]
        Comma -> do
          let layout = if isVertical then PI.nest else PI.idOrNest
          PI.arrange
            [ PI.inject prefix',
              PI.inject $ D.text open,
              layout $ arranger $ commaSeq (elems series) isVertical (trailingComment series),
              PI.inject $ D.text close
            ]

isVerticalSeries :: Series (D.Doc, T.Text, D.Doc) -> Bool
isVerticalSeries series = do
  let b1 = hasOptionalSeparator series
  let b2 = hasComment series
  let (domList, _, codList) = unzip3 $ extract series
  let b3 = D.isMulti $ domList ++ codList
  b1 || b2 || b3

decode' :: Series (D.Doc, T.Text, D.Doc) -> D.Doc
decode' series = do
  if isVerticalSeries series
    then _decode True $ fmap decodeClauseItemVertical series
    else _decode False $ fmap decodeClauseItemHorizontal series

decodeClauseItemVertical :: (D.Doc, T.Text, D.Doc) -> D.Doc
decodeClauseItemVertical (dom, delim, cod) = do
  let domLayout = if D.isMulti [dom] then PI.inject else PI.horizontal
  PI.arrange
    [ domLayout dom,
      PI.inject $ D.text delim,
      PI.inject D.line,
      PI.inject cod
    ]

decodeClauseItemHorizontal :: (D.Doc, T.Text, D.Doc) -> D.Doc
decodeClauseItemHorizontal (dom, delim, cod) = do
  PI.arrange
    [ PI.horizontal dom,
      PI.horizontal $ D.text delim,
      PI.inject cod
    ]

commaSeq :: [(C, D.Doc)] -> Bool -> C -> [PI.Piece]
commaSeq elems isVertical trailingComment = do
  let elems' = map (uncurry attachComment) elems
  commaSeq' elems' isVertical trailingComment

commaSeq' :: [D.Doc] -> Bool -> C -> [PI.Piece]
commaSeq' elems isVertical trailingComment = do
  let separator = getSeparator Comma
  case elems of
    [] ->
      [PI.inject $ C.decode trailingComment]
    [d] -> do
      if isVertical
        then
          [ PI.inject d,
            PI.inject $ D.text separator,
            PI.inject $ C.asSuffix trailingComment
          ]
        else [PI.appendCommaIfVertical d, PI.inject $ C.asSuffix trailingComment]
    d : rest -> do
      [PI.inject d, PI.delimiterLeftAligned $ D.text separator] ++ commaSeq' rest isVertical trailingComment

barSeq :: [(C, D.Doc)] -> Bool -> C -> [PI.Piece]
barSeq elems isVertical trailingComment = do
  case elems of
    [] ->
      if null trailingComment
        then [PI.inject D.Nil]
        else [PI.inject $ D.join [D.text (T.replicate D.indent " "), C.decode trailingComment]]
    (c, d) : rest -> do
      let prefix = if isVertical then PI.inject $ D.text (getSeparator Bar <> " ") else PI.inject D.Nil
      let headElem = [PI.inject $ C.asPrefix' c, prefix, PI.inject (D.nest D.indent d)]
      let rest' = barSeq' rest trailingComment
      headElem ++ rest'

barSeq' :: [(C, D.Doc)] -> C -> [PI.Piece]
barSeq' elems trailingComment = do
  let separator = getSeparator Bar
  case elems of
    [] ->
      [PI.inject $ D.nest D.indent $ C.asSuffix trailingComment]
    (c, d) : rest -> do
      [ PI.inject $ C.asClauseHeader c,
        PI.delimiterBar $ D.text separator,
        PI.inject $ D.nest D.indent d
        ]
        ++ barSeq' rest trailingComment

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
  null (trailingComment series) && isHorizontalSeries' (elems series) && not (hasOptionalSeparator series)

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
