module Scene.Parse.Import (parseImportBlock) where

import Context.Alias qualified as Alias
import Context.Tag qualified as Tag
import Context.Throw qualified as Throw
import Control.Monad.Trans
import Data.HashMap.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.AliasInfo qualified as AI
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.GlobalLocator qualified as GL
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Entity.Module
import Entity.ModuleAlias (ModuleAlias (ModuleAlias))
import Entity.Source qualified as Source
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
import Path
import Scene.Module.Reflect qualified as Module
import Scene.Parse.Core (commaList)
import Scene.Parse.Core qualified as P
import Scene.Source.ShiftToLatest
import Text.Megaparsec

type LocatorText =
  T.Text

parseImportBlock :: Source.Source -> P.Parser [(Source.Source, [AI.AliasInfo])]
parseImportBlock currentSource = do
  m <- P.getCurrentHint
  locatorAndSourceInfo <-
    choice
      [ P.keyword "import" >> P.betweenBrace (P.manyList (parseImport (Source.sourceModule currentSource))),
        return []
      ]
  let (foundLocators, sourceInfo) = unzip locatorAndSourceInfo
  coreSourceInfo <- loadDefaultImports m currentSource foundLocators
  return $ sourceInfo ++ coreSourceInfo

parseImport :: Module -> P.Parser (LocatorText, (Source.Source, [AI.AliasInfo]))
parseImport currentModule = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  (locatorText', mPrefixInfo, source, strictGlobalLocator) <- parseLocatorText currentModule m locatorText
  mUseInfo <- optional $ parseLocalLocatorList strictGlobalLocator
  return (locatorText', (source, catMaybes [mUseInfo, mPrefixInfo]))

parseLocalLocatorList :: SGL.StrictGlobalLocator -> P.Parser AI.AliasInfo
parseLocalLocatorList sgl = do
  lls <-
    choice
      [ P.betweenBracket $ commaList parseLocalLocator,
        P.betweenBrace $ P.manyList parseLocalLocator
      ]
  return $ AI.Use sgl lls

parseLocalLocator :: P.Parser (Hint, LL.LocalLocator)
parseLocalLocator = do
  mLL <- P.getCurrentHint
  ll <- P.baseName
  return (mLL, LL.new ll)

parseLocatorText ::
  Module ->
  Hint ->
  T.Text ->
  P.Parser (T.Text, Maybe AI.AliasInfo, Source.Source, SGL.StrictGlobalLocator)
parseLocatorText currentModule m locatorText = do
  (locatorText', mPrefix, moduleAlias, sourceLocator) <- parseLocatorText' currentModule m locatorText
  sgl <- lift $ Alias.resolveLocatorAlias m moduleAlias sourceLocator
  source <- getSource m sgl locatorText >>= lift . shiftToLatest
  let mAliasInfo = mPrefix >>= \prefix -> return $ AI.Prefix m (GLA.GlobalLocatorAlias prefix) sgl
  return (locatorText', mAliasInfo, source, sgl)

parseLocatorText' ::
  Module ->
  Hint ->
  T.Text ->
  P.Parser (T.Text, Maybe BN.BaseName, ModuleAlias, SL.SourceLocator)
parseLocatorText' currentModule m locatorText = do
  baseNameList <- lift $ Throw.liftEither $ BN.bySplit m locatorText
  case baseNameList of
    [] ->
      lift $ Throw.raiseCritical m "Scene.Parse.Import: empty parse locator"
    [prefix] -> do
      case Map.lookup prefix (modulePrefixMap currentModule) of
        Nothing ->
          lift $ Throw.raiseError m $ "no such prefix is defined: " <> BN.reify prefix
        Just (moduleAlias, sourceLocator) -> do
          let locatorText' = GL.reify $ GL.GlobalLocator moduleAlias sourceLocator
          return (locatorText', Just prefix, moduleAlias, sourceLocator)
    aliasText : locator ->
      case SL.fromBaseNameList locator of
        Nothing ->
          lift $ Throw.raiseError m $ "couldn't parse the locator: " <> locatorText
        Just sourceLocator -> do
          let moduleAlias = ModuleAlias aliasText
          return (locatorText, Nothing, moduleAlias, sourceLocator)

getSource :: Hint -> SGL.StrictGlobalLocator -> LocatorText -> P.Parser Source.Source
getSource m sgl locatorText = do
  nextModule <- lift $ Module.getModule m (SGL.moduleID sgl) locatorText
  relPath <- lift $ addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  let nextPath = getSourceDir nextModule </> relPath
  lift $ Tag.insert m (T.length locatorText) (newSourceHint nextPath)
  return $
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = nextPath,
        Source.sourceHint = Just m
      }

loadDefaultImports :: Hint -> Source.Source -> [LocatorText] -> P.Parser [(Source.Source, [AI.AliasInfo])]
loadDefaultImports m source foundLocators =
  if not (Source.hasCore source)
    then return []
    else do
      let locatorSet = S.fromList foundLocators
      let defaultImports' = filter (\(x, _) -> S.notMember x locatorSet) BN.defaultImports
      mapM (uncurry $ interpretDefaultImport (Source.sourceModule source) m) defaultImports'

interpretDefaultImport :: Module -> Hint -> T.Text -> [BN.BaseName] -> P.Parser (Source.Source, [AI.AliasInfo])
interpretDefaultImport currentModule m globalLocatorText localLocatorBaseNameList = do
  (_, mPrefixInfo, source, sgl) <- parseLocatorText currentModule m globalLocatorText
  let lls = map LL.new localLocatorBaseNameList
  return (source, catMaybes [Just (AI.Use sgl (map (m,) lls)), mPrefixInfo])
