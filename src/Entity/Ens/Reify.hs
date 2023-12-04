module Entity.Ens.Reify (pp) where

import Control.Comonad.Cofree
import Data.Text qualified as T
import Entity.Doc qualified as D
import Entity.Ens

pp :: Cofree EnsF a -> T.Text
pp ens =
  D.layout $ D.join [toDoc ens, D.line]

toDoc :: Cofree EnsF a -> D.Doc
toDoc ens =
  case ens of
    _ :< Int x ->
      D.text $ T.pack (show x)
    _ :< Float x ->
      D.text $ T.pack (show x)
    _ :< Bool x ->
      if x
        then D.text "true"
        else D.text "false"
    _ :< String x ->
      D.text $ T.pack (show x)
    _ :< List xs -> do
      if null xs
        then D.text "[]"
        else do
          let prefix = D.text "["
          let suffix = D.text "]"
          let xs' = map toDoc xs
          let xs'' = map (\x -> D.nest D.baseOffset $ D.join [D.line, x]) xs'
          D.join $ prefix : xs'' ++ [D.line, suffix]
    _ :< Dictionary dict -> do
      if null dict
        then D.text "{}"
        else do
          let prefix = D.text "{"
          let suffix = D.text "}"
          let dictList' = flip map dict $ \(key, value) -> do
                let (commentList, value') = getPrecedingComments value
                let commentList' = map (\comment -> D.join [D.line, D.text $ "//" <> comment]) commentList
                D.nest D.baseOffset $ D.join $ commentList' ++ [D.line, D.text key, D.text " ", toDoc value']
          D.join $ prefix : dictList' ++ [D.line, suffix]
    _ :< Comment comment x -> do
      commentToDoc (D.text comment) (toDoc x)

commentToDoc :: D.Doc -> D.Doc -> D.Doc
commentToDoc comment doc = do
  D.join [D.text "//", comment, D.line, doc]

getPrecedingComments :: Cofree EnsF a -> ([T.Text], Cofree EnsF a)
getPrecedingComments ens =
  case ens of
    _ :< Comment comment ens' -> do
      let (commentList, ens'') = getPrecedingComments ens'
      (comment : commentList, ens'')
    _ ->
      ([], ens)
