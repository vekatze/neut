module Scene.Parse.Import (parseImportBlock) where

import Context.Alias qualified as Alias
import Context.App
import Context.Tag qualified as Tag
import Context.Throw qualified as Throw
import Context.UnusedImport qualified as UnusedImport
import Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Context.UnusedPreset qualified as UnusedPreset
import Control.Monad
import Control.Monad.Trans
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.AliasInfo qualified as AI
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Entity.Module
import Entity.ModuleAlias (ModuleAlias (ModuleAlias))
import Entity.ModuleID qualified as MID
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
  choice
    [ do
        P.keyword "import"
        concat <$> P.betweenBrace (P.manyList (parseImport (Source.sourceModule currentSource))),
      return []
    ]

parseImport :: Module -> P.Parser [(Source.Source, [AI.AliasInfo])]
parseImport currentModule = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  localLocatorList <- parseLocalLocatorList'
  lift $ interpretImportItem True currentModule m locatorText localLocatorList

parseLocalLocatorList' :: P.Parser [(Hint, LL.LocalLocator)]
parseLocalLocatorList' = do
  choice
    [ P.betweenBracket $ commaList parseLocalLocator,
      P.betweenBrace $ P.manyList parseLocalLocator,
      return []
    ]

parseLocalLocator :: P.Parser (Hint, LL.LocalLocator)
parseLocalLocator = do
  m <- P.getCurrentHint
  ll <- P.baseName
  return (m, LL.new ll)

interpretImportItem ::
  Bool ->
  Module ->
  Hint ->
  LocatorText ->
  [(Hint, LL.LocalLocator)] ->
  App [(Source.Source, [AI.AliasInfo])]
interpretImportItem shouldUpdateTag currentModule m locatorText localLocatorList = do
  baseNameList <- Throw.liftEither $ BN.bySplit m locatorText
  case baseNameList of
    [] ->
      Throw.raiseCritical m "Scene.Parse.Import: empty parse locator"
    [baseName]
      | Just (moduleAlias, sourceLocator) <- Map.lookup baseName (modulePrefixMap currentModule) -> do
          sgl <- Alias.resolveLocatorAlias m moduleAlias sourceLocator
          source <- getSource m sgl locatorText
          let gla = GLA.GlobalLocatorAlias baseName
          return [(source, [AI.Use sgl localLocatorList, AI.Prefix m gla sgl])]
      | Just (_, digest) <- Map.lookup (ModuleAlias baseName) (moduleDependency currentModule) -> do
          unless (null localLocatorList) $ do
            Throw.raiseError m "found a non-empty locator list when using alias import"
          nextModule <- Module.getModule m (MID.Library digest) locatorText
          let presetInfo = Map.toList $ modulePresetMap nextModule
          UnusedPreset.insert (MID.reify $ moduleID nextModule) m
          fmap concat $ forM presetInfo $ \(presetSourceLocator, presetLocalLocatorList) -> do
            let newLocatorText = BN.reify baseName <> nsSep <> presetSourceLocator
            let presetLocalLocatorList' = map ((m,) . LL.new) presetLocalLocatorList
            interpretImportItem False nextModule m newLocatorText presetLocalLocatorList'
      | otherwise ->
          Throw.raiseError m $ "no such prefix or alias is defined: " <> BN.reify baseName
    aliasText : locator ->
      case SL.fromBaseNameList locator of
        Nothing ->
          Throw.raiseError m $ "couldn't parse the locator: " <> locatorText
        Just sourceLocator -> do
          let moduleAlias = ModuleAlias aliasText
          sgl <- Alias.resolveLocatorAlias m moduleAlias sourceLocator
          when shouldUpdateTag $ do
            UnusedImport.insert (SGL.reify sgl) m locatorText
            forM_ localLocatorList $ \(ml, ll) -> UnusedLocalLocator.insert ll ml
          source <- getSource m sgl locatorText
          return [(source, [AI.Use sgl localLocatorList])]

getSource :: Hint -> SGL.StrictGlobalLocator -> LocatorText -> App Source.Source
getSource m sgl locatorText = do
  nextModule <- Module.getModule m (SGL.moduleID sgl) locatorText
  relPath <- addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  let nextPath = getSourceDir nextModule </> relPath
  Tag.insertFileLoc m (T.length locatorText) (newSourceHint nextPath)
  shiftToLatest $
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = nextPath,
        Source.sourceHint = Just m
      }
