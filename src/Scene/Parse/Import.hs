module Scene.Parse.Import (parseImportBlock) where

import Context.Alias qualified as Alias
import Context.Throw qualified as Throw
import Control.Monad.Trans
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
      [ P.keyword "import" >> P.betweenBrace (P.manyList parseImport),
        return []
      ]
  let (foundLocators, sourceInfo) = unzip locatorAndSourceInfo
  coreSourceInfo <- loadDefaultImports m currentSource foundLocators
  return $ sourceInfo ++ coreSourceInfo

parseImport :: P.Parser (LocatorText, (Source.Source, [AI.AliasInfo]))
parseImport = do
  m <- P.getCurrentHint
  locatorText <- P.symbol
  (source, strictGlobalLocator) <- parseLocatorText m locatorText
  mUseInfo <- optional $ parseLocalLocatorList strictGlobalLocator
  mPrefixInfo <- optional $ parseAlias strictGlobalLocator
  return (locatorText, (source, catMaybes [mUseInfo, mPrefixInfo]))

parseLocalLocatorList :: SGL.StrictGlobalLocator -> P.Parser AI.AliasInfo
parseLocalLocatorList sgl = do
  lls <-
    choice
      [ P.betweenBracket $ commaList parseLocalLocator,
        P.betweenBrace $ P.manyList parseLocalLocator
      ]
  return $ AI.Use sgl lls

parseAlias :: SGL.StrictGlobalLocator -> P.Parser AI.AliasInfo
parseAlias sgl = do
  P.delimiter "=>"
  m <- P.getCurrentHint
  alias <- GLA.GlobalLocatorAlias <$> P.baseName
  return $ AI.Prefix m alias sgl

parseLocalLocator :: P.Parser (Hint, LL.LocalLocator)
parseLocalLocator = do
  mLL <- P.getCurrentHint
  ll <- P.baseName
  return (mLL, LL.new ll)

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

loadDefaultImports :: Hint -> Source.Source -> [LocatorText] -> P.Parser [(Source.Source, [AI.AliasInfo])]
loadDefaultImports m source foundLocators =
  if not (Source.hasCore source)
    then return []
    else do
      let locatorSet = S.fromList foundLocators
      let defaultImports' = filter (\(x, _) -> S.notMember x locatorSet) BN.defaultImports
      mapM (uncurry $ interpretDefaultImport m) defaultImports'

interpretDefaultImport :: Hint -> T.Text -> [BN.BaseName] -> P.Parser (Source.Source, [AI.AliasInfo])
interpretDefaultImport m globalLocatorText localLocatorBaseNameList = do
  (source, sgl) <- parseLocatorText m globalLocatorText
  let lls = map LL.new localLocatorBaseNameList
  return (source, [AI.Use sgl (map (m,) lls)])
