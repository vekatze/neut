module Scene.Parse.Import
  ( parseImportSequence,
    skimImportSequence,
  )
where

import Context.Alias qualified as Alias
import Context.Throw qualified as Throw
import Control.Monad.Trans
import Data.Either
import Data.Text qualified as T
import Entity.AliasInfo qualified as AI
import Entity.Const
import Entity.GlobalLocator qualified as GL
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.Hint
import Entity.Module
import Entity.Source qualified as Source
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
import Path
import Scene.Module.Reflect qualified as Module
import Scene.Parse.Core qualified as P
import Text.Megaparsec

parseImportSequence :: P.Parser [Source.Source]
parseImportSequence = do
  let p1 = P.importBlock (P.manyList parseSingleImport)
  let p2 = return []
  choice [p1, p2]

parseSingleImport :: P.Parser Source.Source
parseSingleImport = do
  choice
    [ try parseImportQualified,
      parseImportSimple
    ]

skimImportSequence :: P.Parser ([SGL.StrictGlobalLocator], [AI.AliasInfo])
skimImportSequence = do
  choice
    [ fmap partitionEithers $ P.importBlock $ P.manyList skimSingleImport,
      return ([], [])
    ]

skimSingleImport :: P.Parser (Either SGL.StrictGlobalLocator AI.AliasInfo)
skimSingleImport =
  choice
    [ Right <$> try parseImportQualified',
      Left <$> parseImportSimple'
    ]

parseImportSimple :: P.Parser Source.Source
parseImportSimple = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  globalLocator <- lift $ Throw.liftEither $ GL.reflect m locatorText
  strictGlobalLocator <- lift $ Alias.resolveAlias m globalLocator
  getSource m strictGlobalLocator locatorText

-- return (source, Nothing)

parseImportSimple' :: P.Parser SGL.StrictGlobalLocator
parseImportSimple' = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  globalLocator <- lift $ Throw.liftEither $ GL.reflect m locatorText
  lift $ Alias.resolveAlias m globalLocator

-- source <- getSource m strictGlobalLocator locatorText
-- return (source, Nothing)

parseImportQualified :: P.Parser Source.Source
parseImportQualified = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  P.delimiter "=>"
  _ <- GLA.GlobalLocatorAlias <$> P.baseName
  globalLocator <- lift $ Throw.liftEither $ GL.reflect m locatorText
  strictGlobalLocator <- lift $ Alias.resolveAlias m globalLocator
  getSource m strictGlobalLocator locatorText

parseImportQualified' :: P.Parser AI.AliasInfo
parseImportQualified' = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  P.delimiter "=>"
  globalLocatorAlias <- GLA.GlobalLocatorAlias <$> P.baseName
  globalLocator <- lift $ Throw.liftEither $ GL.reflect m locatorText
  strictGlobalLocator <- lift $ Alias.resolveAlias m globalLocator
  return $ AI.Prefix m globalLocatorAlias strictGlobalLocator

getSource :: Hint -> SGL.StrictGlobalLocator -> T.Text -> P.Parser Source.Source
getSource m sgl locatorText = do
  nextModule <- lift $ Module.getModule m (SGL.moduleID sgl) locatorText
  relPath <- lift $ addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  return $
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = getSourceDir nextModule </> relPath
      }
