module Scene.Parse.Import
  ( parseImportSequence,
    skimImportSequence,
  )
where

import Context.Alias qualified as Alias
import Context.Global
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
import Scene.Source.ShiftToLatest
import Text.Megaparsec

parseImportSequence :: Source.Source -> P.Parser [Source.Source]
parseImportSequence currentSource = do
  let p1 = P.importBlock (P.manyList parseSingleImport)
  let p2 = return []
  baseChildren <- choice [p1, p2]
  additionalChildren <- loadDefaultImports currentSource
  return $ baseChildren ++ additionalChildren

parseSingleImport :: P.Parser Source.Source
parseSingleImport = do
  choice
    [ try parseImportQualified,
      parseImportSimple
    ]

skimImportSequence :: Source.Source -> P.Parser ([SGL.StrictGlobalLocator], [AI.AliasInfo])
skimImportSequence currentSource = do
  (baseGlobalLocatorInfo, baseAliasInfo) <-
    choice
      [ fmap partitionEithers $ P.importBlock $ P.manyList skimSingleImport,
        return ([], [])
      ]
  additionalGlobalLocatorInfo <- skimDefaultImports currentSource
  return (baseGlobalLocatorInfo ++ additionalGlobalLocatorInfo, baseAliasInfo)

skimSingleImport :: P.Parser (Either SGL.StrictGlobalLocator AI.AliasInfo)
skimSingleImport =
  choice
    [ Right <$> try parseImportQualified',
      fmap Left $ do
        parseImportSimple'
    ]

parseImportSimple :: P.Parser Source.Source
parseImportSimple = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  importByLocator m locatorText

importByLocator :: Hint -> T.Text -> P.Parser Source.Source
importByLocator m locatorText = do
  globalLocator <- lift $ Throw.liftEither $ GL.reflect m locatorText
  strictGlobalLocator <- lift $ Alias.resolveAlias m globalLocator
  getSource m strictGlobalLocator locatorText

parseImportSimple' :: P.Parser SGL.StrictGlobalLocator
parseImportSimple' = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  getUseInfoByLocator m locatorText

getUseInfoByLocator :: Hint -> T.Text -> P.Parser SGL.StrictGlobalLocator
getUseInfoByLocator m locatorText = do
  globalLocator <- lift $ Throw.liftEither $ GL.reflect m locatorText
  strictGlobalLocator <- lift $ Alias.resolveAlias m globalLocator
  activateTopLevelNames m strictGlobalLocator locatorText
  return strictGlobalLocator

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
  activateTopLevelNames m strictGlobalLocator locatorText
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

activateTopLevelNames :: Hint -> SGL.StrictGlobalLocator -> T.Text -> P.Parser ()
activateTopLevelNames m strictGlobalLocator locatorText = do
  source <- getSource m strictGlobalLocator locatorText >>= lift . shiftToLatest
  lift $ activateTopLevelNamesInSource m source

skimDefaultImports :: Source.Source -> P.Parser [SGL.StrictGlobalLocator]
skimDefaultImports source =
  if not (Source.hasCore source)
    then return []
    else do
      m <- P.getCurrentHint
      mapM (getUseInfoByLocator m) defaultImports

loadDefaultImports :: Source.Source -> P.Parser [Source.Source]
loadDefaultImports source =
  if not (Source.hasCore source)
    then return []
    else do
      m <- P.getCurrentHint
      mapM (importByLocator m) defaultImports

defaultImports :: [T.Text]
defaultImports =
  [ "core.bool",
    "core.bottom",
    "core.i8-array",
    "core.list",
    "core.option",
    "core.sum",
    "core.text",
    "core.top",
    "core.vector"
  ]
