module Entity.C.Decode
  ( decode,
    asPrefix,
    asPrefix',
    asSuffix,
  )
where

import Entity.C
import Entity.Doc qualified as D

decode :: C -> D.Doc
decode =
  D.intercalate D.line . map (\com -> D.join [D.text "//", D.text com])

asPrefix :: C -> D.Doc
asPrefix c =
  if null c
    then D.Nil
    else D.join [decode c, D.line]

asPrefix' :: C -> D.Doc
asPrefix' c =
  if null c
    then D.Nil
    else D.join [D.text " ", decode c, D.line]

asSuffix :: C -> D.Doc
asSuffix c =
  if null c
    then D.Nil
    else D.join [D.line, decode c]
