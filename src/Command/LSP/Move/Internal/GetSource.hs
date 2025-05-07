module Command.LSP.Move.Internal.GetSource
  ( Handle,
    new,
    getSource,
  )
where

import Command.LSP.Move.Internal.Source.Reflect qualified as SourceReflect
import Control.Lens hiding (Iso, List)
import Error.Move.Run (liftMaybe)
import Error.Rule.EIO (EIO)
import Kernel.Move.Scene.Init.Base qualified as Base
import Kernel.Common.Rule.Source (Source)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types

newtype Handle = Handle
  { sourceReflectHandle :: SourceReflect.Handle
  }

new :: Base.Handle -> Handle
new baseHandle = do
  let sourceReflectHandle = SourceReflect.new baseHandle
  Handle {..}

getSource ::
  Handle ->
  (J.HasTextDocument p a1, J.HasUri a1 Uri) =>
  p ->
  EIO Source
getSource h params = do
  fp <- liftMaybe $ uriToFilePath $ params ^. J.textDocument . J.uri
  SourceReflect.reflect (sourceReflectHandle h) fp >>= liftMaybe
