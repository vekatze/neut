module Kernel.Common.RawImportSummary
  ( RawImportSummary,
    RawImportSummaryItem (..),
    SummaryEntry (..),
    fromRawImport,
  )
where

import Data.Binary
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import GHC.Generics
import Language.Common.BaseName qualified as BN
import Language.Common.GlobalLocator qualified as GL
import Language.Common.LocalLocator qualified as LL
import Language.RawTerm.RawStmt
import Logger.Hint
import SyntaxTree.Series qualified as SE

type RawImportSummary = ([RawImportSummaryItem], Loc)

data RawImportSummaryItem = RawImportSummaryItem
  { importLocator :: T.Text,
    importEntries :: [SummaryEntry]
  }
  deriving (Generic)

instance Binary RawImportSummaryItem

data SummaryEntry
  = SummaryEntry (Maybe T.Text) T.Text
  deriving (Generic)

instance Binary SummaryEntry

fromRawImport :: RawImport -> RawImportSummary
fromRawImport (RawImport _ _ itemSeries loc) =
  (catMaybes $ SE.extract $ fmap fromRawImportItem itemSeries, loc)

fromRawImportItem :: RawImportItem -> Maybe RawImportSummaryItem
fromRawImportItem rawImportItem = do
  case rawImportItem of
    RawImportItem m (gl, _) entries -> do
      let normalizedLocator = either (const gl) GL.reify $ GL.reflect m gl
      let entries' = map fromRawImportEntry $ SE.extract entries
      Just RawImportSummaryItem {importLocator = normalizedLocator, importEntries = entries'}
    RawStaticFileKey {} ->
      Nothing

fromRawImportEntry :: RawImportEntry -> SummaryEntry
fromRawImportEntry entry =
  case entry of
    RawImportWildcard _ (RawAsClause _ _ _ importAlias) ->
      SummaryEntry Nothing (BN.reify importAlias)
    RawImportName _ ll asClauseOrNone -> do
      let name = LL.reify ll
      SummaryEntry (Just name) $ maybe name (\(RawAsClause _ _ _ importAlias) -> BN.reify importAlias) asClauseOrNone
