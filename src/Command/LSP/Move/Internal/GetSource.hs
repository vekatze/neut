module Command.LSP.Move.Internal.GetSource
  ( Handle,
    new,
    getSource,
  )
where

import Command.LSP.Move.Internal.Source.Reflect qualified as SourceReflect
import Control.Lens hiding (Iso, List)
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Rule.Source (Source)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Library.Error.Move.Run (liftMaybe)
import Library.Error.Rule.EIO (EIO)

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
  EIO Source
getSource h params = do
  fp <- liftMaybe $ uriToFilePath $ params ^. J.textDocument . J.uri
  SourceReflect.reflect (sourceReflectHandle h) fp >>= liftMaybe
