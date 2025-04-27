module Move.Scene.LSP.Highlight (highlight) where

import Control.Lens hiding (Iso, List)
import Control.Monad.Trans
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Move.Context.AppM
import Move.Scene.LSP.FindDefinition qualified as FindDefinition
import Move.Scene.LSP.FindReferences qualified as LSP

highlight ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  p ->
  AppM [DocumentHighlight]
highlight params = do
  h <- lift FindDefinition.new
  ((_, defLink@(DefinitionLink (LocationLink {_targetRange, _targetUri}))), locTree) <- FindDefinition.findDefinition h params
  let reqUri = params ^. J.textDocument . J.uri
  refs <- lift $ LSP.findReferences defLink locTree
  if reqUri /= _targetUri
    then return refs
    else do
      let _kind = Just DocumentHighlightKind_Write
      return $ DocumentHighlight {_range = _targetRange, _kind} : refs
