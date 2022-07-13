module Scene.Parse.Import
  ( parseImportSequence,
    skipImportSequence,
    Context (..),
  )
where

import qualified Context.Alias as Alias
import qualified Context.Module as Module
import qualified Context.Throw as Throw
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text as T
import Entity.AliasInfo
import Entity.Const
import qualified Entity.GlobalLocator as GL
import qualified Entity.GlobalLocatorAlias as GLA
import Entity.Hint
import Entity.Module
import Entity.Source
import qualified Entity.SourceLocator as SL
import qualified Entity.StrictGlobalLocator as SGL
import Path
import Scene.Parse.Core
import Text.Megaparsec

data Context = Context
  { throw :: Throw.Context,
    moduleCtx :: Module.Context,
    alias :: Alias.Context
  }

parseImportSequence :: Context -> Parser ([Source], [AliasInfo])
parseImportSequence ctx = do
  let p1 = importBlock (manyList $ parseSingleImport ctx)
  let p2 = return []
  (sourceList, mInfoList) <- unzip <$> (p1 <|> p2)
  return (sourceList, catMaybes mInfoList)

parseSingleImport :: Context -> Parser (Source, Maybe AliasInfo)
parseSingleImport ctx = do
  choice
    [ try $ parseImportQualified ctx,
      parseImportSimple ctx
    ]

skipSingleImport :: Parser ()
skipSingleImport = do
  choice
    [ try skipImportQualified,
      skipImportSimple
    ]

skipImportSequence :: Parser ()
skipImportSequence = do
  choice
    [ void $ importBlock $ manyList skipSingleImport,
      return ()
    ]

parseImportSimple :: Context -> Parser (Source, Maybe AliasInfo)
parseImportSimple ctx = do
  m <- currentHint
  locatorText <- symbol
  globalLocator <- liftIO $ GL.reflect (throw ctx) m locatorText
  strictGlobalLocator <- liftIO $ Alias.resolveAlias (alias ctx) m globalLocator
  source <- liftIO $ getSource (moduleCtx ctx) m strictGlobalLocator locatorText
  return (source, Nothing)

skipImportSimple :: Parser ()
skipImportSimple = do
  _ <- symbol
  return ()

parseImportQualified :: Context -> Parser (Source, Maybe AliasInfo)
parseImportQualified ctx = do
  m <- currentHint
  locatorText <- symbol
  keyword "as"
  globalLocator <- liftIO $ GL.reflect (throw ctx) m locatorText
  strictGlobalLocator <- liftIO $ Alias.resolveAlias (alias ctx) m globalLocator
  source <- liftIO $ getSource (moduleCtx ctx) m strictGlobalLocator locatorText
  globalLocatorAlias <- GLA.GlobalLocatorAlias <$> baseName
  return (source, Just $ AliasInfoPrefix m globalLocatorAlias strictGlobalLocator)

skipImportQualified :: Parser ()
skipImportQualified = do
  _ <- symbol
  keyword "as"
  _ <- symbol
  return ()

getSource :: Module.Context -> Hint -> SGL.StrictGlobalLocator -> T.Text -> IO Source
getSource ctx m sgl locatorText = do
  nextModule <- Module.getModule ctx m (SGL.moduleID sgl) locatorText
  relPath <- addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  return $
    Source
      { sourceModule = nextModule,
        sourceFilePath = getSourceDir nextModule </> relPath
      }
