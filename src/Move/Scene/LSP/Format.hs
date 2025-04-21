module Move.Scene.LSP.Format (format) where

import Move.Context.AppM
import Move.Context.Throw qualified as Throw
import Control.Monad.Trans
import Data.Maybe
import Data.Text qualified as T
import Rule.AppLsp
import Rule.Const
import Rule.FileType qualified as FT
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS
import Path
import Path.IO
import Move.Scene.Format qualified as Format
import Move.Scene.LSP.Util (liftAppM)

format :: Format.ShouldMinimizeImports -> Uri -> AppLsp a [TextEdit]
format shouldMinimizeImports uri = do
  fileOrNone <- getVirtualFile (toNormalizedUri uri)
  case fileOrNone of
    Nothing ->
      return []
    Just file -> do
      textEditOrNone <- liftAppM $ _format shouldMinimizeImports uri file
      return $ fromMaybe [] textEditOrNone

_format ::
  Format.ShouldMinimizeImports ->
  Uri ->
  VirtualFile ->
  AppM [TextEdit]
_format shouldMinimizeImports uri file = do
  path <- liftMaybe (uriToFilePath uri) >>= lift . resolveFile'
  newText <- getFormattedContent shouldMinimizeImports file path
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

getFormattedContent :: Format.ShouldMinimizeImports -> VirtualFile -> Path Abs File -> AppM T.Text
getFormattedContent shouldMinimizeImports file path = do
  (_, ext) <- liftMaybe $ splitExtension path
  case (ext == sourceFileExtension, ext == ensFileExtension) of
    (True, _) -> do
      lift (Throw.runMaybe $ Format.format shouldMinimizeImports FT.Source path (virtualFileText file)) >>= liftMaybe
    (_, True) -> do
      lift (Throw.runMaybe $ Format.format shouldMinimizeImports FT.Ens path (virtualFileText file)) >>= liftMaybe
    _ ->
      liftMaybe Nothing
