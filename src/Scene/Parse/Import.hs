module Scene.Parse.Import
  ( parseImportSequence,
    skipImportSequence,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Entity.AliasInfo
import Entity.Module
import Entity.Source
import Scene.Parse.Core
import Text.Megaparsec

parseImportSequence :: Module -> Parser ([Source], [AliasInfo])
parseImportSequence currentModule = do
  unzip
    <$> choice
      [ importBlock $ manyList $ parseSingleImport currentModule,
        return []
      ]

parseSingleImport :: Module -> Parser (Source, AliasInfo)
parseSingleImport currentModule = do
  choice
    [ try $ parseImportQualified currentModule,
      parseImportSimple currentModule
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

parseImportSimple :: Module -> Parser (Source, AliasInfo)
parseImportSimple currentModule = do
  m <- currentHint
  sigText <- symbol
  source <- liftIO $ getNextSource m currentModule sigText
  return (source, AliasInfoUse sigText)

skipImportSimple :: Parser ()
skipImportSimple = do
  _ <- symbol
  return ()

parseImportQualified :: Module -> Parser (Source, AliasInfo)
parseImportQualified currentModule = do
  m <- currentHint
  sigText <- symbol
  keyword "as"
  alias <- symbol
  source <- liftIO $ getNextSource m currentModule sigText
  return (source, AliasInfoPrefix m alias sigText)

skipImportQualified :: Parser ()
skipImportQualified = do
  _ <- symbol
  keyword "as"
  _ <- symbol
  return ()
