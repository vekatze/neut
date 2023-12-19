module Entity.Syntax.Series
  ( Series (..),
    Separator (..),
    Container (..),
    assoc,
    getContainerPair,
    getSeparator,
  )
where

import Data.Text qualified as T
import Entity.C (C)

data Separator
  = Comma
  | Hyphen

data Container
  = Paren
  | Brace
  | Bracket
  | Angle

data Series a = Series
  { elems :: [(C, a)],
    trailingComment :: C,
    separator :: Separator,
    container :: Container
  }

assoc :: Series (a, C) -> Series a
assoc series = do
  let Series {elems, trailingComment, separator, container} = series
  let (elems', trailingComment') = _assoc elems trailingComment
  Series {elems = elems', trailingComment = trailingComment', separator, container}

_assoc :: [(C, (a, C))] -> C -> ([(C, a)], C)
_assoc es c =
  case es of
    [] ->
      ([], c)
    [(c1, (e, c2))] ->
      ([(c1, e)], c2 ++ c)
    (c11, (e1, c12)) : (c21, (e2, c22)) : rest -> do
      let (_assoced, c') = _assoc ((c12 ++ c21, (e2, c22)) : rest) c
      ((c11, e1) : _assoced, c')

getContainerPair :: Container -> (T.Text, T.Text)
getContainerPair container =
  case container of
    Paren ->
      ("(", ")")
    Brace ->
      ("{", "}")
    Bracket ->
      ("[", "]")
    Angle ->
      ("<", ">")

getSeparator :: Separator -> T.Text
getSeparator sep =
  case sep of
    Comma ->
      ","
    Hyphen ->
      "-"
