module Scene.LSP.Format (format) where

import Context.AppM
import Context.Cache (invalidate)
import Context.Throw qualified as Throw
import Control.Monad.Trans
import Data.Maybe
import Data.Text qualified as T
import Entity.AppLsp
import Entity.Const
import Entity.FileType qualified as FT
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS
import Path
import Path.IO
import Scene.Format qualified as Format
import Scene.Source.Reflect qualified as Source

format :: Uri -> AppLsp a [TextEdit]
format uri = do
  fileOrNone <- getVirtualFile (toNormalizedUri uri)
  case fileOrNone of
    Nothing ->
      return []
    Just file -> do
      textEditOrNone <- lift $ runAppM $ _format uri file
      return $ fromMaybe [] textEditOrNone

_format ::
  Uri ->
  VirtualFile ->
  AppM [TextEdit]
_format uri file = do
  path <- liftMaybe (uriToFilePath uri) >>= lift . resolveFile'
  newText <- getFormattedContent file path
  return
    [ TextEdit
        { _range =
            Range
              { _start = Position {_line = 0, _character = 0},
                _end = Position {_line = 2 ^ (31 :: Integer) - 1, _character = 0}
              },
          _newText = newText
        }
    ]

getFormattedContent :: VirtualFile -> Path Abs File -> AppM T.Text
getFormattedContent file path = do
  (_, ext) <- liftMaybe $ splitExtension path
  case (ext == sourceFileExtension, ext == ensFileExtension) of
    (True, _) -> do
      src <- lift (Source.reflect (toFilePath path)) >>= liftMaybe
      lift $ invalidate src
      lift (Throw.runMaybe $ Format.format FT.Source path (virtualFileText file)) >>= liftMaybe
    (_, True) -> do
      lift (Throw.runMaybe $ Format.format FT.Ens path (virtualFileText file)) >>= liftMaybe
    _ ->
      liftMaybe Nothing
