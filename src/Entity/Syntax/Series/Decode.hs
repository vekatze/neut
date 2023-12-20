module Entity.Syntax.Series.Decode (decode) where

import Entity.C
import Entity.C.Decode qualified as C
import Entity.Doc qualified as D
import Entity.Syntax.Series

decode :: Series D.Doc -> D.Doc
decode series = do
  let prefix' = decodePrefix series
  let (open, close) = getContainerPair $ container series
  let elems' = decodeElems (elems series) (trailingComment series)
  case (container series, null (elems series)) of
    (Angle, True) ->
      D.Nil
    _ ->
      D.join
        [ prefix',
          D.text open,
          intercalate (separator series) elems',
          D.text close
        ]

intercalate :: Separator -> [D.Doc] -> D.Doc
intercalate sep elems = do
  case sep of
    Comma ->
      if D.isMulti elems
        then D.join [D.line, D.nest D.indent $ D.commaSeqV elems, D.line]
        else D.commaSeqH elems
    Hyphen ->
      D.join [D.line, D.listSeq elems]

decodeElems :: [(C, D.Doc)] -> C -> [D.Doc]
decodeElems elems trail =
  case elems of
    [] ->
      [C.decode trail]
    (c, d) : rest -> do
      if null c
        then d : decodeElems rest trail
        else [C.decode c, d] ++ decodeElems rest trail

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
              C.decode c
            ]
