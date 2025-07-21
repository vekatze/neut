module Command.LSP.Internal.Highlight
  ( Handle,
    new,
    highlight,
  )
where

import App.App (App)
import Command.LSP.Internal.FindDefinition qualified as FindDefinition
import Command.LSP.Internal.FindReferences qualified as LSP
import Control.Lens hiding (Iso, List)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types

newtype Handle = Handle
  { findDefinitionHandle :: FindDefinition.Handle
  }

new :: Global.Handle -> Handle
new globalHandle = do
  let findDefinitionHandle = FindDefinition.new globalHandle
  Handle {..}

highlight ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  Handle ->
  p ->
  App [DocumentHighlight]
highlight h params = do
  ((_, defLink@(DefinitionLink (LocationLink {_targetRange, _targetUri}))), locTree) <-
    FindDefinition.findDefinition (findDefinitionHandle h) params
  let reqUri = params ^. J.textDocument . J.uri
  let refs = LSP.findReferences defLink locTree
  if reqUri /= _targetUri
    then return refs
    else do
      let _kind = Just DocumentHighlightKind_Write
      return $ DocumentHighlight {_range = _targetRange, _kind} : refs
