module Entity.Ens.Reify (pp) where

import Control.Comonad.Cofree
import Data.Char (isSpace)
import Data.Text qualified as T
import Entity.C
import Entity.Doc qualified as D
import Entity.Ens

pp :: FullEns -> T.Text
pp (leadingComments, (ens, trailingComments)) = do
  let header = commentToDoc leadingComments ++ [D.line]
  let body = [toDoc ens]
  let footer = commentToDoc trailingComments ++ [D.line]
  T.dropWhile isSpace $ D.layout $ D.join $ header ++ body ++ footer

toDoc :: Ens -> D.Doc
toDoc ens =
  case ens of
    _ :< Int x ->
      D.text $ T.pack (show x)
    _ :< Float x ->
      D.text $ T.pack (show x)
    _ :< Bool x -> do
      if x
        then D.text "true"
        else D.text "false"
    _ :< String x ->
      D.text $ T.pack (show x)
    _ :< List c xs -> do
      if null xs && null c
        then D.text "[]"
        else do
          let header = [D.text "["]
          let body = map (D.nest D.indent) (listItemsToDocs c xs)
          let footer = [D.line, D.text "]"]
          D.join $ header ++ body ++ footer
    _ :< Dictionary c dict -> do
      if null dict && null c
        then D.text "{}"
        else do
          let header = [D.text "{"]
          let body = map (D.nest D.indent) (dictItemsToDocs c dict)
          let footer = [D.line, D.text "}"]
          D.join $ header ++ body ++ footer

listItemsToDocs :: C -> [(Ens, C)] -> [D.Doc]
listItemsToDocs c xcs =
  case xcs of
    [] ->
      commentToDoc c
    (x, c') : rest -> do
      commentToDoc c ++ [D.line, toDoc x] ++ listItemsToDocs c' rest

dictItemsToDocs :: C -> [(T.Text, (C, (Ens, C)))] -> [D.Doc]
dictItemsToDocs c kvcs =
  case kvcs of
    [] ->
      commentToDoc c
    (k, (cLead, (v, cTrail))) : rest -> do
      commentToDoc (c ++ cLead) ++ [D.line, D.text k, D.text " ", toDoc v] ++ dictItemsToDocs cTrail rest

commentToDoc :: C -> [D.Doc]
commentToDoc c = do
  foldr (\com acc -> [D.line, D.text "//", D.text com] ++ acc) [] c
