module Main.Move.Scene.LSP.GetSource
  ( Handle,
    new,
    getSource,
  )
where

import Control.Lens hiding (Iso, List)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Main.Move.Context.EIO (EIO, liftMaybe)
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.LSP.Source.Reflect qualified as SourceReflect
import Main.Rule.Source (Source)

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
