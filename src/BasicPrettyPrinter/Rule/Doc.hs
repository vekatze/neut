module BasicPrettyPrinter.Rule.Doc
  ( Doc (..),
    indent,
    line,
    text,
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
  deriving (Show)

indent :: Int
indent =
  2

line :: Doc
line = Line 0 Nil

text :: T.Text -> Doc
text t = Text t Nil

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

nest :: Int -> Doc -> Doc
nest i doc =
  case doc of
    Nil ->
      Nil
    Text t x ->
      Text t (nest i x)
    Line j x ->
      Line (i + j) (nest i x)

layout :: Doc -> T.Text
layout doc =
  case doc of
    Nil ->
      ""
    Text t x ->
      t <> layout x
    Line i x ->
      "\n" <> T.replicate i " " <> layout x

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

intercalate :: Doc -> [Doc] -> Doc
intercalate sep docList =
  case docList of
    [] ->
      Nil
    [doc] ->
      doc
    doc : rest ->
      join [doc, sep, intercalate sep rest]
