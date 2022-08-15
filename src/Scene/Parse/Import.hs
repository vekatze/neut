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
import Control.Monad.Catch hiding (try)
import Control.Monad.Trans
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
import Text.Megaparsec

class (Throw.Context m, Module.Context m, Alias.Context m, P.Context m, MonadThrow m) => Context m

parseImportSequence :: Context m => P.Parser m ([Source.Source], [AliasInfo])
parseImportSequence = do
  let p1 = P.importBlock (P.manyList parseSingleImport)
  let p2 = return []
  (sourceList, mInfoList) <- unzip <$> choice [p1, p2]
  -- (sourceList, mInfoList) <- unzip <$> (p1 <|> p2)
  return (sourceList, catMaybes mInfoList)

parseSingleImport :: Context m => P.Parser m (Source.Source, Maybe AliasInfo)
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

parseImportSimple :: Context m => P.Parser m (Source.Source, Maybe AliasInfo)
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

parseImportQualified :: Context m => P.Parser m (Source.Source, Maybe AliasInfo)
parseImportQualified = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  P.keyword "as"
  globalLocator <- lift $ GL.reflect m locatorText
  strictGlobalLocator <- lift $ Alias.resolveAlias m globalLocator
  source <- getSource m strictGlobalLocator locatorText
  globalLocatorAlias <- GLA.GlobalLocatorAlias <$> P.baseName
  return (source, Just $ AliasInfoPrefix m globalLocatorAlias strictGlobalLocator)

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
