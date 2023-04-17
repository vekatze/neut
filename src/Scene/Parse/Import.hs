module Scene.Parse.Import (parseImportBlock) where

import Context.Alias qualified as Alias
import Context.Throw qualified as Throw
import Control.Monad.Trans
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
import Scene.Source.ShiftToLatest
import Text.Megaparsec

parseImportBlock :: Source.Source -> P.Parser [(Source.Source, AI.AliasInfo)]
parseImportBlock currentSource = do
  sourceInfo <- P.importBlock (P.manyList parseImport) <|> return []
  coreSourceInfo <- loadDefaultImports currentSource
  return $ sourceInfo ++ coreSourceInfo

parseImport :: P.Parser (Source.Source, AI.AliasInfo)
parseImport =
  choice
    [ try parseImportWithAlias,
      parseImportWithoutAlias
    ]

parseImportWithAlias :: P.Parser (Source.Source, AI.AliasInfo)
parseImportWithAlias = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  P.delimiter "=>"
  globalLocatorAlias <- GLA.GlobalLocatorAlias <$> P.baseName
  (source, strictGlobalLocator) <- parseLocatorText m locatorText
  return (source, AI.Prefix m globalLocatorAlias strictGlobalLocator)

parseImportWithoutAlias :: P.Parser (Source.Source, AI.AliasInfo)
parseImportWithoutAlias = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  (source, strictGlobalLocator) <- parseLocatorText m locatorText
  return (source, AI.Use strictGlobalLocator)

parseLocatorText :: Hint -> T.Text -> P.Parser (Source.Source, SGL.StrictGlobalLocator)
parseLocatorText m locatorText = do
  globalLocator <- lift $ Throw.liftEither $ GL.reflect m locatorText
  strictGlobalLocator <- lift $ Alias.resolveAlias m globalLocator
  source <- getSource m strictGlobalLocator locatorText >>= lift . shiftToLatest
  return (source, strictGlobalLocator)

getSource :: Hint -> SGL.StrictGlobalLocator -> T.Text -> P.Parser Source.Source
getSource m sgl locatorText = do
  nextModule <- lift $ Module.getModule m (SGL.moduleID sgl) locatorText
  relPath <- lift $ addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  return $
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = getSourceDir nextModule </> relPath
      }

loadDefaultImports :: Source.Source -> P.Parser [(Source.Source, AI.AliasInfo)]
loadDefaultImports source =
  if not (Source.hasCore source)
    then return []
    else do
      m <- P.getCurrentHint
      sourceInfoList <- mapM (parseLocatorText m) defaultImports
      return $ fmap (fmap AI.Use) sourceInfoList
