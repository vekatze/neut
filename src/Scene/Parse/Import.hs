module Scene.Parse.Import (parseImportBlock) where

import Context.Alias qualified as Alias
import Context.Throw qualified as Throw
import Control.Monad.Trans
import Data.Set qualified as S
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

type LocatorText =
  T.Text

parseImportBlock :: Source.Source -> P.Parser [(Source.Source, AI.AliasInfo)]
parseImportBlock currentSource = do
  locatorAndSourceInfo <-
    choice
      [ P.keyword "import" >> P.betweenBrace (P.manyList parseImport),
        return []
      ]
  let (foundLocators, sourceInfo) = unzip locatorAndSourceInfo
  coreSourceInfo <- loadDefaultImports currentSource foundLocators
  return $ sourceInfo ++ coreSourceInfo

parseImport :: P.Parser (LocatorText, (Source.Source, AI.AliasInfo))
parseImport =
  choice
    [ try parseImportWithAlias,
      parseImportWithoutAlias
    ]

parseImportWithAlias :: P.Parser (LocatorText, (Source.Source, AI.AliasInfo))
parseImportWithAlias = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  P.delimiter "=>"
  globalLocatorAlias <- GLA.GlobalLocatorAlias <$> P.baseName
  (source, strictGlobalLocator) <- parseLocatorText m locatorText
  return (locatorText, (source, AI.Prefix globalLocatorAlias strictGlobalLocator))

parseImportWithoutAlias :: P.Parser (LocatorText, (Source.Source, AI.AliasInfo))
parseImportWithoutAlias = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  (source, strictGlobalLocator) <- parseLocatorText m locatorText
  return (locatorText, (source, AI.Use strictGlobalLocator))

parseLocatorText :: Hint -> T.Text -> P.Parser (Source.Source, SGL.StrictGlobalLocator)
parseLocatorText m locatorText = do
  (moduleAlias, sourceLocator) <- lift $ Throw.liftEither $ GL.reflectLocator m locatorText
  strictGlobalLocator <- lift $ Alias.resolveLocatorAlias m moduleAlias sourceLocator
  source <- getSource m strictGlobalLocator locatorText >>= lift . shiftToLatest
  return (source, strictGlobalLocator)

getSource :: Hint -> SGL.StrictGlobalLocator -> LocatorText -> P.Parser Source.Source
getSource m sgl locatorText = do
  nextModule <- lift $ Module.getModule m (SGL.moduleID sgl) locatorText
  relPath <- lift $ addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  return $
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = getSourceDir nextModule </> relPath,
        Source.sourceHint = Just m
      }

loadDefaultImports :: Source.Source -> [LocatorText] -> P.Parser [(Source.Source, AI.AliasInfo)]
loadDefaultImports source foundLocators =
  if not (Source.hasCore source)
    then return []
    else do
      m <- P.getCurrentHint
      let locatorSet = S.fromList foundLocators
      let defaultImports' = filter (`S.notMember` locatorSet) defaultImports
      sourceInfoList <- mapM (parseLocatorText m) defaultImports'
      return $ fmap (fmap AI.Use) sourceInfoList
