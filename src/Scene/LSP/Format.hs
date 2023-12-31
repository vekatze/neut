module Scene.LSP.Format (format) where

import Context.App
import Context.AppM
import Context.Parse qualified as Parse
import Context.Throw qualified as Throw
import Control.Lens hiding (Iso, List)
import Control.Monad.Trans
import Data.Text qualified as T
import Entity.Const
import Entity.FileType qualified as FT
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Path
import Path.IO
import Scene.Format qualified as Format

format ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri) =>
  p ->
  AppM [TextEdit]
format params = do
  fp <- liftMaybe $ uriToFilePath $ params ^. J.textDocument . J.uri
  path <- resolveFile' fp
  newTextOrNone <- lift $ getFormattedContent path
  case newTextOrNone of
    Nothing ->
      return []
    Just newText -> do
      oldText <- lift (Throw.runMaybe $ Parse.readSourceFile path) >>= liftMaybe
      let lineCount = fromIntegral $ length $ T.lines oldText
      return
        [ TextEdit
            { _range =
                Range
                  { _start = Position {_line = 0, _character = 0},
                    _end = Position {_line = lineCount, _character = 0}
                  },
              _newText = newText
            }
        ]

getFormattedContent :: Path Abs File -> App (Maybe T.Text)
getFormattedContent path = do
  case splitExtension path of
    Just (_, ext)
      | ext == sourceFileExtension -> do
          Throw.runMaybe $ Format.format FT.Source path
      | ext == ensFileExtension ->
          Throw.runMaybe $ Format.format FT.Ens path
    _ ->
      return Nothing
