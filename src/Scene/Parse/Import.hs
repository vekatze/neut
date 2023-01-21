module Scene.Parse.Import
  ( parseImportSequence,
    skipImportSequence,
    Context (),
  )
where

import Context.Alias qualified as Alias
import Context.Module qualified as Module
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.Catch hiding (try)
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
import Scene.Parse.Core qualified as P
import Text.Megaparsec

class (Throw.Context m, Module.Context m, Alias.Context m, P.Context m, MonadThrow m) => Context m

parseImportSequence :: Context m => P.Parser m ([Source.Source], [AI.AliasInfo])
parseImportSequence = do
  let p1 = P.importBlock (P.manyList parseSingleImport)
  let p2 = return []
  (sourceList, mInfoList) <- unzip <$> choice [p1, p2]
  return (sourceList, catMaybes mInfoList)

parseSingleImport :: Context m => P.Parser m (Source.Source, Maybe AI.AliasInfo)
parseSingleImport = do
  choice
    [ try parseImportQualified,
      parseImportSimple
    ]

skipSingleImport :: Context m => P.Parser m ()
skipSingleImport = do
  choice
    [ try skipImportQualified,
      skipImportSimple
    ]

skipImportSequence :: Context m => P.Parser m ()
skipImportSequence = do
  choice
    [ void $ P.importBlock $ P.manyList skipSingleImport,
      return ()
    ]

parseImportSimple :: Context m => P.Parser m (Source.Source, Maybe AI.AliasInfo)
parseImportSimple = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  globalLocator <- lift $ GL.reflect m locatorText
  strictGlobalLocator <- lift $ Alias.resolveAlias m globalLocator
  source <- getSource m strictGlobalLocator locatorText
  return (source, Nothing)

skipImportSimple :: Context m => P.Parser m ()
skipImportSimple = do
  _ <- P.symbol
  return ()

parseImportQualified :: Context m => P.Parser m (Source.Source, Maybe AI.AliasInfo)
parseImportQualified = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  P.keyword "as"
  globalLocator <- lift $ GL.reflect m locatorText
  strictGlobalLocator <- lift $ Alias.resolveAlias m globalLocator
  source <- getSource m strictGlobalLocator locatorText
  globalLocatorAlias <- GLA.GlobalLocatorAlias <$> P.baseName
  return (source, Just $ AI.Prefix m globalLocatorAlias strictGlobalLocator)

skipImportQualified :: Context m => P.Parser m ()
skipImportQualified = do
  _ <- P.symbol
  P.keyword "as"
  _ <- P.symbol
  return ()

getSource :: Context m => Hint -> SGL.StrictGlobalLocator -> T.Text -> P.Parser m Source.Source
getSource m sgl locatorText = do
  nextModule <- lift $ Module.getModule m (SGL.moduleID sgl) locatorText
  relPath <- lift $ addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  return $
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = getSourceDir nextModule </> relPath
      }
