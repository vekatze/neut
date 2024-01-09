module Entity.RawImportSummary
  ( RawImportSummary,
    fromRawImport,
  )
where

import Data.Text qualified as T
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Entity.RawProgram (RawImport (..), RawImportItem (..))
import Entity.Syntax.Series qualified as SE

type RawImportSummary = ([(T.Text, [T.Text])], Loc)

fromRawImport :: RawImport -> RawImportSummary
fromRawImport (RawImport _ _ itemSeries loc) =
  (SE.extract $ fmap fromRawImportItem itemSeries, loc)

fromRawImportItem :: RawImportItem -> (T.Text, [T.Text])
fromRawImportItem (RawImportItem _ (gl, _) llSeries) = do
  let llList = map (LL.reify . snd) $ SE.extract llSeries
  (gl, llList)
