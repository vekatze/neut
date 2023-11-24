module Scene.LSP.Highlight (highlight) where

import Context.AppM
import Control.Lens hiding (Iso, List)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Scene.LSP.FindDefinition qualified as LSP
import Scene.LSP.FindReferences qualified as LSP

highlight ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  p ->
  AppM [DocumentHighlight]
highlight params = do
  (DefinitionLink (LocationLink {_targetRange, _targetUri}), locTree) <- LSP.findDefinition params
  let Range {_start = Position {_line, _character}} = _targetRange
  let line = fromIntegral $ _line + 1
  let col = fromIntegral $ _character + 1
  let reqUri = params ^. J.textDocument . J.uri
  refs <- LSP.findReferences (line, col) _targetUri locTree
  if reqUri /= _targetUri
    then return refs
    else do
      let _kind = Just DocumentHighlightKind_Write
      return $ DocumentHighlight {_range = _targetRange, _kind} : refs
