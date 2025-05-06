module SyntaxTree.Rule.C.ToDoc
  ( decode,
    asPrefix,
    asPrefix',
    asSuffix,
    asClauseHeader,
  )
where

import BasicPrettyPrinter.Rule.Doc qualified as D
import Data.Text qualified as T
import SyntaxTree.Rule.C

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
    else D.join [D.text (T.replicate D.indent " "), D.nest D.indent $ decode c, D.line]

asSuffix :: C -> D.Doc
asSuffix c =
  if null c
    then D.Nil
    else D.join [D.line, decode c]

asClauseHeader :: C -> D.Doc
asClauseHeader c =
  if null c
    then D.Nil
    else D.nest D.indent $ D.join [D.line, decode c]
