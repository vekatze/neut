module Move.Scene.LSP.Highlight
  ( Handle,
    new,
    highlight,
  )
where

import Control.Lens hiding (Iso, List)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Move.Context.App (App)
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.LSP.FindDefinition qualified as FindDefinition
import Move.Scene.LSP.FindReferences qualified as LSP

newtype Handle
  = Handle
  { findDefinitionHandle :: FindDefinition.Handle
  }

new :: Env.Handle -> Gensym.Handle -> Color.Handle -> Debug.Handle -> App Handle
new envHandle gensymHandle colorHandle debugHandle = do
  findDefinitionHandle <- FindDefinition.new envHandle gensymHandle colorHandle debugHandle
  return $ Handle {..}

highlight ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri, J.HasPosition p Position) =>
  Handle ->
  p ->
  EIO [DocumentHighlight]
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
