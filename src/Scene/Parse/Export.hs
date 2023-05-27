module Scene.Parse.Export (parseExportBlock) where

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
    [ P.betweenBrace $ parseExportForVariantWithWildcard nameArrow,
      return $ NA.Function nameArrow
    ]

parseNameArrow :: P.Parser NA.InnerRawNameArrow
parseNameArrow = do
  (m, original) <- P.parseName
  return (m, original)

parseExportForVariantWithWildcard :: NA.InnerRawNameArrow -> P.Parser NA.RawNameArrow
parseExportForVariantWithWildcard dataNameArrow = do
  P.delimiter ".."
  return $ NA.Variant dataNameArrow
