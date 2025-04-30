module Move.Scene.LSP.Format
  ( Handle,
    new,
    format,
  )
where

import Control.Monad.Trans
import Data.Text qualified as T
import Language.LSP.Protocol.Types
import Language.LSP.VFS
import Move.Context.EIO (EIO, liftMaybe)
import Move.Scene.Format qualified as Format
import Path
import Path.IO
import Rule.Const
import Rule.FileType qualified as FT

newtype Handle
  = Handle
  { formatHandle :: Format.Handle
  }

new :: Format.Handle -> Handle
new formatHandle = do
  Handle {..}

format :: Handle -> Format.ShouldMinimizeImports -> Uri -> Maybe VirtualFile -> EIO [TextEdit]
format h shouldMinimizeImports uri fileOrNone = do
  case fileOrNone of
    Nothing ->
      return []
    Just file -> do
      _format h shouldMinimizeImports uri file

_format ::
  Handle ->
  Format.ShouldMinimizeImports ->
  Uri ->
  VirtualFile ->
  EIO [TextEdit]
_format h shouldMinimizeImports uri file = do
  path <- liftMaybe (uriToFilePath uri) >>= lift . resolveFile'
  newText <- getFormattedContent h shouldMinimizeImports file path
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

getFormattedContent :: Handle -> Format.ShouldMinimizeImports -> VirtualFile -> Path Abs File -> EIO T.Text
getFormattedContent h shouldMinimizeImports file path = do
  (_, ext) <- liftMaybe $ splitExtension path
  case (ext == sourceFileExtension, ext == ensFileExtension) of
    (True, _) -> do
      Format.format (formatHandle h) shouldMinimizeImports FT.Source path (virtualFileText file)
    (_, True) -> do
      Format.format (formatHandle h) shouldMinimizeImports FT.Ens path (virtualFileText file)
    _ ->
      liftMaybe Nothing
