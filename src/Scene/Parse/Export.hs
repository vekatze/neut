module Scene.Parse.Export (parseExportBlock) where

import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Control.Monad.Trans
import Entity.BaseName qualified as BN
import Entity.DefiniteDescription qualified as DD
import Entity.Error
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Entity.NameArrow qualified as NA
import Entity.VarOrLocator
import Scene.Parse.Core qualified as P
import Scene.Parse.RawTerm qualified as P
import Text.Megaparsec

parseExportBlock :: P.Parser [NA.RawNameArrow]
parseExportBlock = do
  choice
    [ P.keyword "export" >> P.betweenBrace (P.manyList parseExport),
      return []
    ]

parseExport :: P.Parser NA.RawNameArrow
parseExport =
  choice
    [ try parseExportForVariant,
      NA.Function <$> parseExport'
    ]

parseExport' :: P.Parser NA.InnerRawNameArrow
parseExport' =
  choice
    [ try parseExportWithAlias,
      parseExportWithoutAlias
    ]

parseExportWithAlias :: P.Parser NA.InnerRawNameArrow
parseExportWithAlias = do
  (mOrig, originalName) <- P.parseVarOrLocator
  P.delimiter "=>"
  mAlias <- P.getCurrentHint
  specifiedAlias <- P.baseName
  aliasDD <- lift $ Locator.attachPublicCurrentLocator specifiedAlias -- exported names are public
  return ((mAlias, aliasDD), (mOrig, originalName))

parseExportForVariant :: P.Parser NA.RawNameArrow
parseExportForVariant = do
  dataClause <- parseExport'
  clauseList <- P.betweenBrace $ P.manyList parseExport'
  return $ NA.Variant dataClause clauseList

parseExportWithoutAlias :: P.Parser NA.InnerRawNameArrow
parseExportWithoutAlias = do
  (m, original) <- P.parseVarOrLocator
  autoAliasDD <- getAutoAlias m original
  return ((m, autoAliasDD), (m, original))

getAutoAlias :: Hint -> VarOrLocator -> P.Parser DD.DefiniteDescription
getAutoAlias m varOrLocator = do
  baseName <- lift $ Throw.liftEither $ getBaseName m varOrLocator
  lift $ Locator.attachPublicCurrentLocator baseName -- exported names are public

getBaseName :: Hint -> VarOrLocator -> Either Error BN.BaseName
getBaseName m varOrLocator =
  case varOrLocator of
    Var var ->
      BN.reflect m var
    Locator _ ll ->
      Right $ LL.baseName ll
