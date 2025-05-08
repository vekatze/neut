module Aux.Ens.Rule.Ens.ToDoc (pp) where

import Aux.Ens.Rule.Ens
import Aux.PrettyPrinter.Rule.Doc qualified as D
import Aux.SyntaxTree.Rule.C
import Aux.SyntaxTree.Rule.Series.ToDoc qualified as SE
import Control.Comonad.Cofree
import Data.Char (isSpace)
import Data.Text qualified as T

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
    _ :< List xs -> do
      let xs' = fmap toDoc xs
      SE.decode xs'
    _ :< Dictionary dict -> do
      SE.decode $ fmap dictItemToDoc dict

dictItemToDoc :: (T.Text, Ens) -> D.Doc
dictItemToDoc (k, v) =
  D.join [D.text k, D.text " ", toDoc v]

commentToDoc :: C -> [D.Doc]
commentToDoc c = do
  foldr (\com acc -> [D.line, D.text "//", D.text com] ++ acc) [] c
