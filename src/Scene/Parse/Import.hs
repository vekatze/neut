module Scene.Parse.Import
  ( parseImportSequence,
    skipImportSequence,
    Context (..),
  )
where

import qualified Context.Path as Path
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
import qualified Entity.Module.Locator as Module
import Entity.ModuleAlias
import Entity.Source
import qualified Entity.SourceLocator as SL
import Path
import Scene.Parse.Core
import Text.Megaparsec

data Context = Context
  { throw :: Throw.Context,
    path :: Path.Context
  }

parseImportSequence :: Context -> Module -> Parser ([Source], [AliasInfo])
parseImportSequence ctx currentModule = do
  let p1 = importBlock (manyList $ parseSingleImport ctx currentModule)
  let p2 = return []
  (sourceList, mInfoList) <- unzip <$> (p1 <|> p2)
  return (sourceList, catMaybes mInfoList)

parseSingleImport :: Context -> Module -> Parser (Source, Maybe AliasInfo)
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

parseImportSimple :: Context -> Module -> Parser (Source, Maybe AliasInfo)
parseImportSimple ctx currentModule = do
  m <- currentHint
  sigText <- symbol
  (moduleAlias, sourceLocator) <- liftIO $ GL.reflect' (throw ctx) sigText
  source <- liftIO $ getNextSource ctx m currentModule moduleAlias sourceLocator
  return (source, Nothing)

skipImportSimple :: Parser ()
skipImportSimple = do
  _ <- symbol
  return ()

parseImportQualified :: Context -> Module -> Parser (Source, Maybe AliasInfo)
parseImportQualified ctx currentModule = do
  m <- currentHint
  sigText <- symbol
  keyword "as"
  alias <- GLA.GlobalLocatorAlias <$> symbol
  (moduleAlias, sourceLocator) <- liftIO $ GL.reflect' (throw ctx) sigText
  source <- liftIO $ getNextSource ctx m currentModule moduleAlias sourceLocator
  return (source, Just $ AliasInfoPrefix m alias (GL.GlobalLocator moduleAlias sourceLocator))

skipImportQualified :: Parser ()
skipImportQualified = do
  _ <- symbol
  keyword "as"
  _ <- symbol
  return ()

getNextSource :: Context -> Hint -> Module -> ModuleAlias -> SL.SourceLocator -> IO Source
getNextSource ctx m currentModule moduleAlias sourceLocator = do
  let moduleCtx = Module.Context {Module.throw = throw ctx, Module.path = path ctx}
  nextModule <- Module.getNextModule moduleCtx m currentModule moduleAlias
  let relFile = SL.reify sourceLocator <> nsSep <> sourceFileExtension
  relPath <- parseRelFile $ T.unpack relFile
  return $
    Source
      { sourceModule = nextModule,
        sourceFilePath = getSourceDir nextModule </> relPath
      }
