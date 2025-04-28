module Move.Scene.LSP.GetSource
  ( Handle,
    new,
    getSource,
  )
where

import Control.Lens hiding (Iso, List)
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Move.Context.App (App)
import Move.Context.EIO (EIO, liftMaybe)
import Move.Context.Env qualified as Env
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Source.Reflect qualified as SourceReflect
import Rule.Source (Source)

newtype Handle
  = Handle
  { sourceReflectHandle :: SourceReflect.Handle
  }

new :: Env.Handle -> Gensym.Handle -> App Handle
new envHandle gensymHandle = do
  sourceReflectHandle <- SourceReflect.new envHandle gensymHandle
  return $ Handle {..}

getSource ::
  Handle ->
  (J.HasTextDocument p a1, J.HasUri a1 Uri) =>
  p ->
  EIO Source
getSource h params = do
  fp <- liftMaybe $ uriToFilePath $ params ^. J.textDocument . J.uri
  SourceReflect.reflect (sourceReflectHandle h) fp >>= liftMaybe
