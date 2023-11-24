module Scene.LSP.Highlight (highlight) where

import Context.App
import Control.Lens hiding (Iso, List)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Scene.LSP.FindDefinition qualified as LSP
import Scene.LSP.FindReferences qualified as LSP

highlight ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  p ->
  App (Maybe [DocumentHighlight])
highlight params = do
  mDefLoc <- LSP.findDefinition params
  case mDefLoc of
    Nothing ->
      return Nothing
    Just (DefinitionLink (LocationLink {_targetRange, _targetUri}), locTree) -> do
      let Range {_start = Position {_line, _character}} = _targetRange
      let line = fromIntegral $ _line + 1
      let col = fromIntegral $ _character + 1
      let reqUri = params ^. J.textDocument . J.uri
      refs <- LSP.findReferences (line, col) _targetUri locTree
      if reqUri /= _targetUri
        then return $ Just refs
        else do
          let _kind = Just DocumentHighlightKind_Write
          return $ Just $ DocumentHighlight {_range = _targetRange, _kind} : refs
