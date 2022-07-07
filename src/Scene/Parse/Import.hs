module Scene.Parse.Import
  ( parseImportSequence,
    skipImportSequence,
  )
where

import qualified Context.Throw as Throw
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import qualified Data.Text as T
import Entity.AliasInfo
import Entity.Global
import Entity.Hint
import Entity.Module
import Entity.Module.Locator
import Entity.ModuleAlias
import Entity.Source
import Path
import Scene.Parse.Core
import Text.Megaparsec

parseImportSequence :: Throw.Context -> Module -> Parser ([Source], [AliasInfo])
parseImportSequence ctx currentModule = do
  unzip
    <$> choice
      [ importBlock $ manyList $ parseSingleImport ctx currentModule,
        return []
      ]

parseSingleImport :: Throw.Context -> Module -> Parser (Source, AliasInfo)
parseSingleImport ctx currentModule = do
  choice
    [ try $ parseImportQualified ctx currentModule,
      parseImportSimple ctx currentModule
    ]

skipSingleImport :: Parser ()
skipSingleImport = do
  choice
    [ try skipImportQualified,
      skipImportSimple
    ]

skipImportSequence :: Parser ()
skipImportSequence = do
  choice
    [ void $ importBlock $ manyList skipSingleImport,
      return ()
    ]

parseImportSimple :: Throw.Context -> Module -> Parser (Source, AliasInfo)
parseImportSimple ctx currentModule = do
  m <- currentHint
  sigText <- symbol
  source <- liftIO $ parseLocator ctx m currentModule sigText
  return (source, AliasInfoUse sigText)

skipImportSimple :: Parser ()
skipImportSimple = do
  _ <- symbol
  return ()

parseImportQualified :: Throw.Context -> Module -> Parser (Source, AliasInfo)
parseImportQualified ctx currentModule = do
  m <- currentHint
  sigText <- symbol
  keyword "as"
  alias <- symbol
  source <- liftIO $ parseLocator ctx m currentModule sigText
  return (source, AliasInfoPrefix m alias sigText)

skipImportQualified :: Parser ()
skipImportQualified = do
  _ <- symbol
  keyword "as"
  _ <- symbol
  return ()

parseLocator :: Throw.Context -> Hint -> Module -> T.Text -> IO Source
parseLocator ctx m currentModule locatorString = do
  case uncons $ T.splitOn "." locatorString of
    Just (nextModuleName, locatorTail) -> do
      nextModule <- getNextModule ctx m currentModule $ ModuleAlias nextModuleName
      let relFile = T.intercalate "/" locatorTail <> nsSep <> sourceFileExtension
      relPath <- parseRelFile $ T.unpack relFile
      return $
        Source
          { sourceModule = nextModule,
            sourceFilePath = getSourceDir nextModule </> relPath
          }
    Nothing ->
      Throw.raiseError ctx m "found a malformed module signature"
