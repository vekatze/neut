module Language.RawTerm.Name
  ( Name (..),
    reflect,
    showName,
    isConsName,
  )
where

import App.Error (Error)
import Data.Char
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Common.BaseName qualified as BN
import Language.Common.Const (nsSep)
import Language.Common.NamePath qualified as NP
import Language.RawTerm.Locator qualified as L
import Language.RawTerm.Name.Error qualified as NameError
import Language.RawTerm.RawIdent
import Logger.Hint qualified as H

data Name
  = Bare RawIdent
  | Dotted RawIdent (NonEmpty BN.BaseName) -- head.rest-1.….rest-n; no `::`
  | Locator L.Locator
  deriving (Show)

reflect :: H.Hint -> T.Text -> Either Error Name
reflect m nameText = do
  case L.reflectMaybe m nameText of
    Left err ->
      Left err
    Right (Just locator) ->
      Right $ Locator locator
    Right Nothing ->
      case T.splitOn nsSep nameText of
        [] ->
          Left $ NameError.unexpectedEnd m nameText "a name"
        [headText]
          | T.null headText ->
              Left $ NameError.unexpectedEnd m nameText "a name"
        segments@(headText : rest)
          | T.null headText ->
              Left $ NameError.unexpectedSeparator m nameText nsSep "a name"
          | T.null (last segments) ->
              Left $ NameError.unexpectedEnd m nameText "a name"
          | any T.null segments ->
              Left $ NameError.unexpectedSeparator m nameText nsSep "a name"
          | otherwise -> do
              reflectNormalizedName m nameText $ NP.normalizeText $ headText : rest

reflectNormalizedName :: H.Hint -> T.Text -> [T.Text] -> Either Error Name
reflectNormalizedName m nameText segments = do
  case segments of
    [] ->
      Left $ NameError.missingDefinition m nameText
    [headText] ->
      Right $ Bare headText
    headText : rest -> do
      rest' <- mapM (BN.reflect m) rest
      case NE.nonEmpty rest' of
        Nothing ->
          Left $ NameError.missingDefinition m nameText
        Just rest'' ->
          Right $ Dotted headText rest''

showName :: Name -> T.Text
showName consName =
  case consName of
    Bare value ->
      value
    Dotted headName rest ->
      T.intercalate nsSep (headName : map BN.reify (NE.toList rest))
    Locator l ->
      L.reify l

isConsName :: T.Text -> Bool
isConsName name = do
  case T.uncons (T.dropWhile (== '_') name) of
    Just (c, _)
      | isUpper c ->
          True
    _ ->
      False
