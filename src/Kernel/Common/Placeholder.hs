module Kernel.Common.Placeholder
  ( Resolver,
    expand,
    mainModuleResolver,
    moduleResolver,
    quote,
    quoteWords,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Monad (foldM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Kernel.Common.Handle.Global.Module qualified as GlobalModule
import Kernel.Common.Module qualified as M
import Kernel.Common.Module.GetModule qualified as GetModule
import Language.Common.BaseName qualified as BN
import Language.Common.ModuleAlias qualified as MA
import Language.Common.ModuleID qualified as MID
import Logger.Hint (Hint)
import Path
import System.Environment (getExecutablePath)

type Resolver = Hint -> T.Text -> App (Maybe T.Text)

expand :: T.Text -> Resolver -> (Hint, T.Text) -> App T.Text
expand fieldName resolve (m, text) = do
  case splitPlaceholder text of
    Nothing ->
      return text
    Just (before, name, after) -> do
      valueOrNone <- resolve m name
      case valueOrNone of
        Nothing ->
          raiseError m $ "No such placeholder is available in `" <> fieldName <> "`: {{" <> name <> "}}"
        Just value -> do
          rest <- expand fieldName resolve (m, after)
          return $ before <> value <> rest

splitPlaceholder :: T.Text -> Maybe (T.Text, T.Text, T.Text)
splitPlaceholder text = do
  case T.breakOn "{{" text of
    (before, rest)
      | T.null rest ->
          Nothing
      | otherwise -> do
          let body = T.drop 2 rest
          case T.breakOn "}}" body of
            (name, closing)
              | not (T.null closing),
                isPlaceholderName name ->
                  Just (before, name, T.drop 2 closing)
              | otherwise -> do
                  (innerBefore, innerName, innerAfter) <- splitPlaceholder body
                  return (before <> "{{" <> innerBefore, innerName, innerAfter)

isPlaceholderName :: T.Text -> Bool
isPlaceholderName name =
  not (T.null name) && T.all isPlaceholderChar name

isPlaceholderChar :: Char -> Bool
isPlaceholderChar c =
  isAlphaNum c || c `elem` ("-_.:" :: String)

quote :: T.Text -> T.Text
quote text =
  "'" <> T.replace "'" "'\\''" text <> "'"

quoteWords :: [T.Text] -> T.Text
quoteWords =
  T.unwords . map quote

mainModuleResolver :: GlobalModule.Handle -> M.MainModule -> Resolver
mainModuleResolver h mainModule =
  moduleResolver h mainModule (M.extractModule mainModule)

moduleResolver :: GlobalModule.Handle -> M.MainModule -> M.Module -> Resolver
moduleResolver h mainModule contextModule m name = do
  case name of
    "neut" -> do
      neutPath <- liftIO getExecutablePath
      return $ Just $ quote $ T.pack neutPath
    _ ->
      case T.stripPrefix "module:" name of
        Nothing ->
          return Nothing
        Just pathText -> do
          moduleRootDir <- resolveModulePath h mainModule contextModule m pathText
          return $ Just $ quote $ T.pack $ toFilePath moduleRootDir

resolveModulePath :: GlobalModule.Handle -> M.MainModule -> M.Module -> Hint -> T.Text -> App (Path Abs Dir)
resolveModulePath h mainModule contextModule m pathText = do
  aliasTextList <- interpretModulePath m pathText
  targetModule <- foldM (stepIntoDependency (GetModule.new h) mainModule m pathText) contextModule aliasTextList
  return $ M.getModuleRootDir targetModule

interpretModulePath :: Hint -> T.Text -> App [T.Text]
interpretModulePath m pathText = do
  let segmentList = T.splitOn "." pathText
  when (any T.null segmentList) $
    raiseError m $ "A module path must not have an empty segment, but found: `" <> pathText <> "`"
  case segmentList of
    first : rest
      | first == MA.reify MA.thisModuleAlias ->
          return rest
    _ ->
      return segmentList

stepIntoDependency :: GetModule.Handle -> M.MainModule -> Hint -> T.Text -> M.Module -> T.Text -> App M.Module
stepIntoDependency h mainModule m pathText currentModule aliasText = do
  aliasName <- either (const $ raiseError m $ "Invalid module alias: " <> aliasText) return $ BN.reflect' aliasText
  let alias = MA.ModuleAlias aliasName
  case Map.lookup alias (M.moduleDependency currentModule) of
    Nothing ->
      raiseError m $
        "The module path `"
          <> pathText
          <> "` mentions `"
          <> aliasText
          <> "`, which is not a dependency of `"
          <> MID.reify (M.moduleID currentModule)
          <> "`"
    Just dependency -> do
      let moduleID = MID.Library $ M.dependencyDigest dependency
      GetModule.getModule h mainModule m moduleID aliasText
