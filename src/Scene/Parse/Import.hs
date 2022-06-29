module Scene.Parse.Import
  ( parseImportSequence,
    skipImportSequence,
  )
where

import Context.App
import Control.Monad
import Control.Monad.IO.Class
import Entity.AliasInfo
import Entity.Module
import Entity.Source
import Scene.Parse.Core
import Text.Megaparsec

parseImportSequence :: Axis -> Module -> Parser ([Source], [AliasInfo])
parseImportSequence axis currentModule = do
  unzip
    <$> choice
      [ importBlock $ manyList $ parseSingleImport axis currentModule,
        return []
      ]

parseSingleImport :: Axis -> Module -> Parser (Source, AliasInfo)
parseSingleImport axis currentModule = do
  choice
    [ try $ parseImportQualified axis currentModule,
      parseImportSimple axis currentModule
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

parseImportSimple :: Axis -> Module -> Parser (Source, AliasInfo)
parseImportSimple axis currentModule = do
  m <- currentHint
  sigText <- symbol
  source <- liftIO $ getNextSource axis m currentModule sigText
  return (source, AliasInfoUse sigText)

skipImportSimple :: Parser ()
skipImportSimple = do
  _ <- symbol
  return ()

parseImportQualified :: Axis -> Module -> Parser (Source, AliasInfo)
parseImportQualified axis currentModule = do
  m <- currentHint
  sigText <- symbol
  keyword "as"
  alias <- symbol
  source <- liftIO $ getNextSource axis m currentModule sigText
  return (source, AliasInfoPrefix m alias sigText)

skipImportQualified :: Parser ()
skipImportQualified = do
  _ <- symbol
  keyword "as"
  _ <- symbol
  return ()
