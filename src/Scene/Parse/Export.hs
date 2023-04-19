module Scene.Parse.Export (parseExportBlock) where

import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Control.Monad.Trans
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.DefiniteDescription qualified as DD
import Entity.Error
import Entity.GlobalLocator qualified as GL
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Scene.Parse.Core qualified as P
import Scene.Parse.RawTerm qualified as P
import Text.Megaparsec

type VarOrDD =
  (Either T.Text (GL.GlobalLocator, LL.LocalLocator))

type AliasName =
  DD.DefiniteDescription

parseExportBlock :: P.Parser [(Hint, AliasName, VarOrDD)]
parseExportBlock = do
  choice
    [ P.keyword "export" >> P.betweenBrace (P.manyList parseExport),
      return []
    ]

parseExport :: P.Parser (Hint, AliasName, VarOrDD)
parseExport =
  choice
    [ try parseExportWithAlias,
      parseExportWithoutAlias
    ]

parseExportWithAlias :: P.Parser (Hint, AliasName, VarOrDD)
parseExportWithAlias = do
  (_, originalName) <- P.parseVarOrDefiniteDescription
  P.delimiter "=>"
  m <- P.getCurrentHint
  specifiedAlias <- P.baseName
  aliasDD <- lift $ Locator.attachCurrentLocator specifiedAlias
  return (m, aliasDD, originalName)

parseExportWithoutAlias :: P.Parser (Hint, AliasName, VarOrDD)
parseExportWithoutAlias = do
  (m, original) <- P.parseVarOrDefiniteDescription
  autoAliasDD <- getAutoAlias m original
  return (m, autoAliasDD, original)

getAutoAlias :: Hint -> VarOrDD -> P.Parser DD.DefiniteDescription
getAutoAlias m varOrDefiniteDescription = do
  baseName <- lift $ Throw.liftEither $ getBaseName m varOrDefiniteDescription
  lift $ Locator.attachCurrentLocator baseName

getBaseName :: Hint -> Either T.Text (GL.GlobalLocator, LL.LocalLocator) -> Either Error BN.BaseName
getBaseName m varOrDefiniteDescription =
  case varOrDefiniteDescription of
    Left var ->
      BN.reflect m var
    Right (_, ll) ->
      Right $ LL.baseName ll
