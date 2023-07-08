module Scene.Tree.Reflect (reflect) where

import Context.App
import Control.Comonad.Cofree
import Entity.Tree
import Path
import Scene.Parse.Core
import Text.Megaparsec

parseSymbol :: Parser Tree
parseSymbol = do
  m <- getCurrentHint
  s <- symbol
  return $ m :< Symbol s

parseString :: Parser Tree
parseString = do
  m <- getCurrentHint
  s <- string
  return $ m :< String s

parseNode :: Parser Tree
parseNode = do
  m <- getCurrentHint
  ts <- betweenParen $ many parseTree
  return $ m :< Node ts

parseTree :: Parser Tree
parseTree =
  choice
    [ parseNode,
      parseString,
      parseSymbol
    ]

parseTreeList :: Parser TreeList
parseTreeList = do
  m <- getCurrentHint
  ts <- many parseTree
  return (m, ts)

reflect :: Path Abs File -> App TreeList
reflect path = do
  run parseTreeList path
