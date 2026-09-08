module Kernel.Common.Template (ensureNoUnknownPlaceholder) where

import App.App (App)
import App.Run (raiseError')
import Control.Monad (forM_)
import Data.Char (isAlphaNum)
import Data.Text qualified as T

ensureNoUnknownPlaceholder :: T.Text -> [T.Text] -> App ()
ensureNoUnknownPlaceholder fieldName textList =
  forM_ textList $ \text ->
    case findPlaceholder text of
      Nothing ->
        return ()
      Just name ->
        raiseError' $ "No such placeholder is available in `" <> fieldName <> "`: {{" <> name <> "}}"

findPlaceholder :: T.Text -> Maybe T.Text
findPlaceholder text =
  case T.breakOn "{{" text of
    (_, rest)
      | T.null rest ->
          Nothing
      | otherwise -> do
          let body = T.drop 2 rest
          case T.breakOn "}}" body of
            (name, closing)
              | not (T.null closing),
                isPlaceholderName name ->
                  Just name
              | otherwise ->
                  findPlaceholder body

isPlaceholderName :: T.Text -> Bool
isPlaceholderName name =
  not (T.null name) && T.all isPlaceholderChar name

isPlaceholderChar :: Char -> Bool
isPlaceholderChar c =
  isAlphaNum c || c `elem` ("-_.:" :: String)
