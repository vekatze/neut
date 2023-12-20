module Entity.Syntax.Series.Decode (decode) where

import Data.Bifunctor
import Entity.C
import Entity.C.Decode qualified as C
import Entity.Doc qualified as D
import Entity.Syntax.Series

decode :: Series D.Doc -> D.Doc
decode series = do
  let prefix' = decodePrefix series
  let (open, close) = getContainerPair $ container series
  case (container series, null (elems series)) of
    (Angle, True) ->
      D.Nil
    _ ->
      D.join
        [ prefix',
          D.text open,
          intercalate (separator series) (elems series) (trailingComment series),
          D.text close
        ]

intercalate :: Separator -> [(C, D.Doc)] -> C -> D.Doc
intercalate sep elems trailingComment = do
  let trailingComment' = C.decode trailingComment
  let elems' = map (first C.decode) elems
  case sep of
    Comma -> do
      let (cs, ds) = unzip elems'
      if D.isMulti (cs ++ ds ++ [trailingComment'])
        then D.join [D.nest D.indent $ D.join [D.line, commaSeqV elems trailingComment], D.line]
        else D.commaSeqH ds
    Hyphen ->
      D.join [listSeq elems trailingComment, D.line]

commaSeqV :: [(C, D.Doc)] -> C -> D.Doc
commaSeqV elems trail =
  case elems of
    [] ->
      C.decode trail
    [(c, d)] -> do
      D.join [C.asPrefix c, d, C.asSuffix trail]
    (c, d) : rest -> do
      D.join [C.asPrefix c, d, D.text ",", D.line, commaSeqV rest trail]

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
