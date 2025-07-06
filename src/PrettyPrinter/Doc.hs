module PrettyPrinter.Doc
  ( Doc (..),
    indent,
    line,
    text,
    inlineComment,
    join,
    nest,
    layout,
    isMulti,
    intercalate,
  )
where

import Data.Text qualified as T

data Doc
  = Nil
  | Text T.Text Doc
  | Line Int Doc
  | InlineComment T.Text Doc
  deriving (Show)

indent :: Int
indent =
  2

line :: Doc
line = Line 0 Nil

text :: T.Text -> Doc
text t = Text t Nil

inlineComment :: T.Text -> Doc
inlineComment t = InlineComment t Nil

join :: [Doc] -> Doc
join =
  foldr _join Nil

_join :: Doc -> Doc -> Doc
_join doc1 doc2 =
  case doc1 of
    Nil ->
      doc2
    Text t x ->
      Text t (_join x doc2)
    Line i x ->
      Line i (_join x doc2)
    InlineComment t x ->
      InlineComment t (_join x doc2)

nest :: Int -> Doc -> Doc
nest i doc =
  case doc of
    Nil ->
      Nil
    Text t x ->
      Text t (nest i x)
    Line j x ->
      Line (i + j) (nest i x)
    InlineComment t x ->
      InlineComment t (nest i x)

layout :: Doc -> T.Text
layout doc =
  layoutDirect (slideInlineComments doc)

slideInlineComments :: Doc -> Doc
slideInlineComments doc =
  case doc of
    Nil ->
      Nil
    Text t x ->
      Text t (slideInlineComments x)
    Line i x ->
      case x of
        -- InlineComment t cont ->
        --   Text (" " <> t) (Line i (slideInlineComments cont))
        InlineComment t cont ->
          Text (" " <> t) (slideInlineComments cont)
        _ ->
          Line i (slideInlineComments x)
    InlineComment t x ->
      InlineComment t (slideInlineComments x)

layoutDirect :: Doc -> T.Text
layoutDirect doc =
  case doc of
    Nil ->
      ""
    Text t x ->
      t <> layoutDirect x
    Line i x ->
      "\n" <> T.replicate i " " <> layoutDirect x
    InlineComment t x ->
      t <> layoutDirect x

isMulti :: [Doc] -> Bool
isMulti docList =
  case docList of
    [] ->
      False
    doc : rest ->
      case doc of
        Nil ->
          isMulti rest
        Text _ next ->
          isMulti $ next : rest
        Line {} ->
          True
        InlineComment _ next ->
          isMulti $ next : rest

intercalate :: Doc -> [Doc] -> Doc
intercalate sep docList =
  case docList of
    [] ->
      Nil
    [doc] ->
      doc
    doc : rest ->
      join [doc, sep, intercalate sep rest]
