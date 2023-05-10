module Scene.Parse.Export (parseExportBlock) where

import Context.App
import Context.Gensym qualified as Gensym
import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Control.Monad.Trans
import Entity.BaseName qualified as BN
import Entity.DefiniteDescription qualified as DD
import Entity.Error
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Entity.Name
import Entity.NameArrow qualified as NA
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
parseExport = do
  nameArrow <- parseNameArrow
  choice
    [ P.betweenBrace $ do
        choice
          [ try $ parseExportForVariantWithWildcard nameArrow,
            parseExportForVariant nameArrow
          ],
      return $ NA.Function nameArrow
    ]

parseNameArrow :: P.Parser NA.InnerRawNameArrow
parseNameArrow =
  choice
    [ try parseExportWithAlias,
      parseExportWithoutAlias
    ]

parseExportWithAlias :: P.Parser NA.InnerRawNameArrow
parseExportWithAlias = do
  (mOrig, originalName) <- P.parseName
  P.delimiter "=>"
  mAlias <- P.getCurrentHint
  specifiedAlias <- P.baseName
  specifiedAlias' <-
    if specifiedAlias /= BN.hole
      then return specifiedAlias
      else do
        unusedVar <- lift Gensym.newTextForHole
        return $ BN.fromText unusedVar
  aliasDD <- lift $ Locator.attachPublicCurrentLocator specifiedAlias' -- exported names are public
  return ((mAlias, aliasDD), (mOrig, originalName))

parseExportForVariant :: NA.InnerRawNameArrow -> P.Parser NA.RawNameArrow
parseExportForVariant dataNameArrow = do
  clauseList <- P.manyList parseNameArrow
  return $ NA.Variant dataNameArrow (NA.Explicit clauseList)

parseExportForVariantWithWildcard :: NA.InnerRawNameArrow -> P.Parser NA.RawNameArrow
parseExportForVariantWithWildcard dataNameArrow = do
  m <- P.getCurrentHint
  P.delimiter ".."
  return $ NA.Variant dataNameArrow $ NA.Automatic m

parseExportWithoutAlias :: P.Parser NA.InnerRawNameArrow
parseExportWithoutAlias = do
  (m, original) <- P.parseName
  autoAliasDD <- lift $ getDefaultAlias m original
  return ((m, autoAliasDD), (m, original))

getDefaultAlias :: Hint -> Name -> App DD.DefiniteDescription
getDefaultAlias m varOrLocator = do
  baseName <- Throw.liftEither $ getBaseName m varOrLocator
  Locator.attachPublicCurrentLocator baseName -- exported names are public

getBaseName :: Hint -> Name -> Either Error BN.BaseName
getBaseName m varOrLocator =
  case varOrLocator of
    Var var ->
      BN.reflect m var
    Locator (_, ll) ->
      Right $ LL.baseName ll
    DefiniteDescription dd ->
      Right $ LL.baseName (DD.localLocator dd)
