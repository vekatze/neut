module Scene.Parse.Import
  ( parseImportSequence,
    skipImportSequence,
  )
where

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
import Entity.Module.Locator
import Entity.ModuleAlias
import Entity.Source
import qualified Entity.SourceLocator as SL
import Path
import Scene.Parse.Core
import Text.Megaparsec

parseImportSequence :: Throw.Context -> Module -> Parser ([Source], [AliasInfo])
parseImportSequence ctx currentModule = do
  let p1 = importBlock (manyList $ parseSingleImport ctx currentModule)
  let p2 = return []
  (sourceList, mInfoList) <- unzip <$> (p1 <|> p2)
  return (sourceList, catMaybes mInfoList)

parseSingleImport :: Throw.Context -> Module -> Parser (Source, Maybe AliasInfo)
parseSingleImport ctx currentModule = do
  choice
    [ try $ parseImportQualified ctx currentModule,
      parseImportSimple ctx currentModule
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

parseImportSimple :: Throw.Context -> Module -> Parser (Source, Maybe AliasInfo)
parseImportSimple ctx currentModule = do
  m <- currentHint
  sigText <- symbol
  (moduleAlias, sourceLocator) <- liftIO $ GL.reflect' ctx sigText
  source <- liftIO $ parseLocator ctx m currentModule moduleAlias sourceLocator
  return (source, Nothing)

skipImportSimple :: Parser ()
skipImportSimple = do
  _ <- symbol
  return ()

parseImportQualified :: Throw.Context -> Module -> Parser (Source, Maybe AliasInfo)
parseImportQualified ctx currentModule = do
  m <- currentHint
  sigText <- symbol
  keyword "as"
  alias <- GLA.GlobalLocatorAlias <$> symbol
  (moduleAlias, sourceLocator) <- liftIO $ GL.reflect' ctx sigText
  source <- liftIO $ parseLocator ctx m currentModule moduleAlias sourceLocator
  return (source, Just $ AliasInfoPrefix m alias (GL.GlobalLocator moduleAlias sourceLocator))

skipImportQualified :: Parser ()
skipImportQualified = do
  _ <- symbol
  keyword "as"
  _ <- symbol
  return ()

parseLocator :: Throw.Context -> Hint -> Module -> ModuleAlias -> SL.SourceLocator -> IO Source
parseLocator ctx m currentModule moduleAlias sourceLocator = do
  nextModule <- getNextModule ctx m currentModule moduleAlias
  let relFile = SL.reify sourceLocator <> nsSep <> sourceFileExtension
  relPath <- parseRelFile $ T.unpack relFile
  return $
    Source
      { sourceModule = nextModule,
        sourceFilePath = getSourceDir nextModule </> relPath
      }
