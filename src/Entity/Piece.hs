module Entity.Piece
  ( Piece (..),
    arrange,
    container,
    symbol,
    delimiter,
    delimiterLeftAligned,
    inject,
  )
where

import Entity.Doc qualified as D

data Piece = Piece
  { content :: D.Doc,
    singleModifier :: D.Doc -> D.Doc,
    multiModifier :: D.Doc -> D.Doc
  }

arrange :: [Piece] -> D.Doc
arrange docList = do
  if D.isMulti $ map content docList
    then D.join $ map _applyMulti $ _removeNil docList
    else D.join $ map _applySingle $ _removeNil docList

_removeNil :: [Piece] -> [Piece]
_removeNil docList =
  case docList of
    [] ->
      []
    Piece {content = D.Nil} : rest ->
      _removeNil rest
    doc : rest ->
      doc : _removeNil rest

container :: D.Doc -> Piece
container doc =
  Piece
    { content = doc,
      singleModifier = id,
      multiModifier = _appendNewLine
    }

delimiter :: D.Doc -> Piece
delimiter doc =
  Piece
    { content = doc,
      singleModifier = _wrapBySpace,
      multiModifier = _appendNewLine
    }

symbol :: D.Doc -> Piece
symbol doc =
  Piece
    { content = doc,
      singleModifier = id,
      multiModifier = _appendNewLine
    }

delimiterLeftAligned :: D.Doc -> Piece
delimiterLeftAligned doc =
  Piece
    { content = doc,
      singleModifier = _appendSpace,
      multiModifier = _appendNewLine
    }

inject :: D.Doc -> Piece
inject doc =
  Piece
    { content = doc,
      singleModifier = id,
      multiModifier = id
    }

_appendNewLine :: D.Doc -> D.Doc
_appendNewLine doc =
  D.join [doc, D.line]

_appendSpace :: D.Doc -> D.Doc
_appendSpace doc =
  D.join [doc, D.text " "]

_wrapBySpace :: D.Doc -> D.Doc
_wrapBySpace doc =
  D.join [D.text " ", doc, D.text " "]

_applySingle :: Piece -> D.Doc
_applySingle (Piece {..}) =
  singleModifier content

_applyMulti :: Piece -> D.Doc
_applyMulti (Piece {..}) =
  multiModifier content
