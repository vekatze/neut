module Command.LSP.Internal.GetSource
  ( Handle,
    new,
    getSource,
  )
where

import App.App (App)
import App.Run (liftMaybe)
import Command.LSP.Internal.Source.Reflect qualified as SourceReflect
import Control.Lens hiding (Iso, List)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Source (Source)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types

newtype Handle = Handle
  { sourceReflectHandle :: SourceReflect.Handle
  }

new :: Global.Handle -> Handle
new globalHandle = do
  let sourceReflectHandle = SourceReflect.new globalHandle
  Handle {..}

getSource ::
  Handle ->
  (J.HasTextDocument p a1, J.HasUri a1 Uri) =>
  p ->
  App Source
getSource h params = do
  fp <- liftMaybe $ uriToFilePath $ params ^. J.textDocument . J.uri
  SourceReflect.reflect (sourceReflectHandle h) fp >>= liftMaybe
