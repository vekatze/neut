module Scene.Parse.Import
  ( parseImportSequence,
    skipImportSequence,
  )
where

import Context.Alias qualified as Alias
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
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

parseImportSequence :: P.Parser ([Source.Source], [AI.AliasInfo])
parseImportSequence = do
  let p1 = P.importBlock (P.manyList parseSingleImport)
  let p2 = return []
  (sourceList, mInfoList) <- unzip <$> choice [p1, p2]
  return (sourceList, catMaybes mInfoList)

parseSingleImport :: P.Parser (Source.Source, Maybe AI.AliasInfo)
parseSingleImport = do
  choice
    [ try parseImportQualified,
      parseImportSimple
    ]

skipSingleImport :: P.Parser ()
skipSingleImport = do
  choice
    [ try skipImportQualified,
      skipImportSimple
    ]

skipImportSequence :: P.Parser ()
skipImportSequence = do
  choice
    [ void $ P.importBlock $ P.manyList skipSingleImport,
      return ()
    ]

parseImportSimple :: P.Parser (Source.Source, Maybe AI.AliasInfo)
parseImportSimple = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  globalLocator <- lift $ Throw.liftEither $ GL.reflect m locatorText
  strictGlobalLocator <- lift $ Alias.resolveAlias m globalLocator
  source <- getSource m strictGlobalLocator locatorText
  return (source, Nothing)

skipImportSimple :: P.Parser ()
skipImportSimple = do
  _ <- P.symbol
  return ()

parseImportQualified :: P.Parser (Source.Source, Maybe AI.AliasInfo)
parseImportQualified = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  P.delimiter "->"
  globalLocator <- lift $ Throw.liftEither $ GL.reflect m locatorText
  strictGlobalLocator <- lift $ Alias.resolveAlias m globalLocator
  source <- getSource m strictGlobalLocator locatorText
  globalLocatorAlias <- GLA.GlobalLocatorAlias <$> P.baseName
  return (source, Just $ AI.Prefix m globalLocatorAlias strictGlobalLocator)

skipImportQualified :: P.Parser ()
skipImportQualified = do
  _ <- P.symbol
  P.delimiter "->"
  _ <- P.symbol
  return ()

getSource :: Hint -> SGL.StrictGlobalLocator -> T.Text -> P.Parser Source.Source
getSource m sgl locatorText = do
  nextModule <- lift $ Module.getModule m (SGL.moduleID sgl) locatorText
  relPath <- lift $ addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  return $
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = getSourceDir nextModule </> relPath
      }
