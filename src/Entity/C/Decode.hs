module Entity.C.Decode (decode) where

import Entity.C
import Entity.Doc qualified as D

decode :: C -> D.Doc
decode c =
  D.join $ foldr (\com acc -> [D.text "//", D.text com, D.line] ++ acc) [] c

-- decAfterDelim :: (a -> D.Doc) -> T.Text -> (C, (a, C)) -> D.Doc
-- decAfterDelim k delim (_, (a, _)) =
--   D.join [D.text delim, k a]
