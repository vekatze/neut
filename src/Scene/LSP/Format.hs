module Scene.LSP.Format (format) where

import Context.App
import Context.AppM
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
  newText <- lift (getFormattedContent file path) >>= liftMaybe
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

getFormattedContent :: VirtualFile -> Path Abs File -> App (Maybe T.Text)
getFormattedContent file path = do
  case splitExtension path of
    Just (_, ext)
      | ext == sourceFileExtension -> do
          Throw.runMaybe $ Format.format FT.Source path (virtualFileText file)
      | ext == ensFileExtension ->
          Throw.runMaybe $ Format.format FT.Ens path (virtualFileText file)
    _ ->
      return Nothing
