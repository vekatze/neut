module SyntaxTree.C.ToDoc
  ( decode,
    asPrefix,
    asPrefix',
    asSuffix,
    asClauseHeader,
    asStmtPrefix,
  )
where

import Data.List (uncons)
import Data.Text qualified as T
import PrettyPrinter.Doc qualified as D
import SyntaxTree.C

decode :: C -> D.Doc
decode commentList =
  case uncons commentList of
    Nothing ->
      D.Nil
    Just (headComment, rest) -> do
      case commentType headComment of
        LineComment -> do
          D.intercalate D.line $ map decode' (headComment : rest)
        InlineComment -> do
          let rest' = D.intercalate D.line $ map decode' rest
          D.join [decode' headComment, rest']

decode' :: Comment -> D.Doc
decode' com = do
  case commentType com of
    LineComment ->
      D.text $ "//" <> commentText com
    InlineComment ->
      D.inlineComment $ "//" <> commentText com

asPrefix :: C -> D.Doc
asPrefix commentList = do
  case uncons commentList of
    Nothing ->
      D.Nil
    Just _ -> do
      let commentList' = map decode' commentList
      D.join [D.intercalate D.line commentList', D.line]

asStmtPrefix :: C -> D.Doc
asStmtPrefix commentList = do
  asPrefix $ map (\com -> com {commentType = LineComment}) commentList

hasLeadingInlineComment :: C -> Bool
hasLeadingInlineComment commentList =
  case uncons commentList of
    Nothing ->
      False
    Just (com, _) ->
      commentType com == InlineComment

asPrefix' :: C -> D.Doc
asPrefix' c =
  if null c
    then D.Nil
    else D.join [D.text (T.replicate D.indent " "), D.nest D.indent $ decode c, D.line]

asSuffix :: C -> D.Doc
asSuffix commentList =
  if null commentList
    then D.Nil
    else do
      let prefix = if hasLeadingInlineComment commentList then D.text " " else D.line
      let commentList' = map decode' commentList
      D.join [prefix, D.intercalate D.line commentList']

asClauseHeader :: C -> D.Doc
asClauseHeader c =
  if null c
    then D.Nil
    else D.nest D.indent $ D.join [D.line, decode c]
