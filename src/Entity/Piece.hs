module Entity.Piece
  ( Piece (..),
    arrange,
    container,
    parameter,
    beforeBareSeries,
    bareSeries,
    delimiter,
    delimiterLeftAligned,
    clauseDelimiter,
    horizontal,
    idOrNest,
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

idOrNest :: D.Doc -> Piece
idOrNest doc =
  Piece
    { content = doc,
      singleModifier = id,
      multiModifier = \d -> D.join [D.nest D.indent $ D.join [D.line, d], D.line]
    }

delimiter :: D.Doc -> Piece
delimiter doc =
  Piece
    { content = doc,
      singleModifier = _wrapBySpace,
      multiModifier = _appendNewLine
    }

clauseDelimiter :: D.Doc -> Piece
clauseDelimiter doc =
  Piece
    { content = doc,
      singleModifier = _wrapBySpace,
      multiModifier = _appendNewLine'
    }

parameter :: D.Doc -> Piece
parameter doc =
  Piece
    { content = doc,
      singleModifier = id,
      multiModifier = id
    }

bareSeries :: D.Doc -> Piece
bareSeries doc =
  Piece
    { content = doc,
      singleModifier = _appendSpace,
      multiModifier = \d -> D.join [D.nest D.indent $ D.join [D.line, d], D.line]
    }

beforeBareSeries :: D.Doc -> Piece
beforeBareSeries doc =
  Piece
    { content = doc,
      singleModifier = _appendSpace,
      multiModifier = id
    }

delimiterLeftAligned :: D.Doc -> Piece
delimiterLeftAligned doc =
  Piece
    { content = doc,
      singleModifier = _appendSpace,
      multiModifier = _appendNewLine
    }

horizontal :: D.Doc -> Piece
horizontal doc =
  Piece
    { content = doc,
      singleModifier = _appendSpace,
      multiModifier = _appendSpace
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

_appendNewLine' :: D.Doc -> D.Doc
_appendNewLine' doc =
  D.join [D.text " ", doc, D.line]

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
