module Scene.Parse.Import
  ( parseImportSequence,
    skipImportSequence,
    Context (),
  )
where

import qualified Context.Alias as Alias
import qualified Context.Module as Module
import qualified Context.Throw as Throw
import Control.Monad
import Control.Monad.Catch
import Data.Maybe
import qualified Data.Text as T
import Entity.AliasInfo
import Entity.Const
import qualified Entity.GlobalLocator as GL
import qualified Entity.GlobalLocatorAlias as GLA
import Entity.Hint
import Entity.Module
import qualified Entity.Source as Source
import qualified Entity.SourceLocator as SL
import qualified Entity.StrictGlobalLocator as SGL
import Path
import qualified Scene.Parse.Core as P

class (Throw.Context m, Module.Context m, Alias.Context m, P.Context m, MonadThrow m) => Context m

parseImportSequence :: Context m => m ([Source.Source], [AliasInfo])
parseImportSequence = do
  let p1 = P.importBlock (P.manyList parseSingleImport)
  let p2 = return []
  (sourceList, mInfoList) <- unzip <$> P.choice [p1, p2]
  -- (sourceList, mInfoList) <- unzip <$> (p1 <|> p2)
  return (sourceList, catMaybes mInfoList)

parseSingleImport :: Context m => m (Source.Source, Maybe AliasInfo)
parseSingleImport = do
  P.choice
    [ P.try parseImportQualified,
      parseImportSimple
    ]

skipSingleImport :: Context m => m ()
skipSingleImport = do
  P.choice
    [ P.try skipImportQualified,
      skipImportSimple
    ]

skipImportSequence :: Context m => m ()
skipImportSequence = do
  P.choice
    [ void $ P.importBlock $ P.manyList skipSingleImport,
      return ()
    ]

parseImportSimple :: Context m => m (Source.Source, Maybe AliasInfo)
parseImportSimple = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  globalLocator <- GL.reflect m locatorText
  strictGlobalLocator <- Alias.resolveAlias m globalLocator
  source <- getSource m strictGlobalLocator locatorText
  return (source, Nothing)

skipImportSimple :: Context m => m ()
skipImportSimple = do
  _ <- P.symbol
  return ()

parseImportQualified :: Context m => m (Source.Source, Maybe AliasInfo)
parseImportQualified = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  P.keyword "as"
  globalLocator <- GL.reflect m locatorText
  strictGlobalLocator <- Alias.resolveAlias m globalLocator
  source <- getSource m strictGlobalLocator locatorText
  globalLocatorAlias <- GLA.GlobalLocatorAlias <$> P.baseName
  return (source, Just $ AliasInfoPrefix m globalLocatorAlias strictGlobalLocator)

skipImportQualified :: Context m => m ()
skipImportQualified = do
  _ <- P.symbol
  P.keyword "as"
  _ <- P.symbol
  return ()

getSource :: Context m => Hint -> SGL.StrictGlobalLocator -> T.Text -> m Source.Source
getSource m sgl locatorText = do
  nextModule <- Module.getModule m (SGL.moduleID sgl) locatorText
  relPath <- addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  return $
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = getSourceDir nextModule </> relPath
      }
