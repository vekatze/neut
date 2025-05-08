module Kernel.Common.Rule.RawImportSummary
  ( RawImportSummary,
    fromRawImport,
  )
where

import Aux.Logger.Rule.Hint
import Aux.SyntaxTree.Rule.Series qualified as SE
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Language.Common.Rule.LocalLocator qualified as LL
import Language.RawTerm.Rule.RawStmt (RawImport (..), RawImportItem (..))

type RawImportSummary = ([(T.Text, [T.Text])], Loc)

fromRawImport :: RawImport -> RawImportSummary
fromRawImport (RawImport _ _ itemSeries loc) =
  (catMaybes $ SE.extract $ fmap fromRawImportItem itemSeries, loc)

fromRawImportItem :: RawImportItem -> Maybe (T.Text, [T.Text])
fromRawImportItem rawImportItem = do
  case rawImportItem of
    RawImportItem _ (gl, _) llSeries -> do
      let llList = map (LL.reify . snd) $ SE.extract llSeries
      Just (gl, llList)
    RawStaticKey {} ->
      Nothing
