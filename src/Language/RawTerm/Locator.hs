module Language.RawTerm.Locator
  ( Locator (..),
    reflect,
    reflectMaybe,
    reify,
  )
where

import App.Error (Error)
import Data.Text qualified as T
import Language.Common.BaseName qualified as BN
import Language.Common.Const
import Language.Common.GlobalLocator qualified as GL
import Language.Common.LocalLocator qualified as LL
import Language.Common.NamePath qualified as NP
import Language.RawTerm.Name.Error qualified as NameError
import Logger.Hint qualified as H

data Locator = Locator
  { modulePathText :: T.Text,
    sourceText :: T.Text,
    globalLocator :: GL.GlobalLocator,
    localLocator :: LL.LocalLocator
  }
  deriving (Show)

reflect :: H.Hint -> T.Text -> Either Error Locator
reflect m addressText = do
  case T.splitOn doubleColon addressText of
    [] ->
      Left $ NameError.unexpectedEnd m addressText "a module path"
    [modulePathText]
      | T.null modulePathText ->
          Left $ NameError.unexpectedEnd m addressText "a module path"
      | otherwise -> do
          _ <- GL.reflectModulePath m modulePathText
          Left $ NameError.unexpectedEnd m addressText "`.` or `::`"
    modulePathText : sourceText : bodyTexts ->
      reflectFromSource m addressText modulePathText sourceText bodyTexts

reflectFromSource :: H.Hint -> T.Text -> T.Text -> T.Text -> [T.Text] -> Either Error Locator
reflectFromSource m addressText modulePathText sourceText bodyTexts = do
  if T.null modulePathText
    then Left $ NameError.unexpectedSeparator m addressText doubleColon "a module path"
    else do
      _ <- GL.reflectModulePath m modulePathText
      case bodyTexts of
        []
          | T.null sourceText ->
              Left $ NameError.unexpectedEnd m addressText "a source path"
          | otherwise -> do
              _ <- reflectGlobalLocator m modulePathText sourceText
              Left $ NameError.unexpectedEnd m addressText "`.` or `::`"
        bodyText : remainingBodyTexts
          | T.null sourceText ->
              Left $ NameError.unexpectedSeparator m addressText doubleColon "a source path"
          | otherwise -> do
              globalLocator <- reflectGlobalLocator m modulePathText sourceText
              reflectBody m addressText modulePathText sourceText globalLocator bodyText remainingBodyTexts

reflectBody :: H.Hint -> T.Text -> T.Text -> T.Text -> GL.GlobalLocator -> T.Text -> [T.Text] -> Either Error Locator
reflectBody m addressText modulePathText sourceText globalLocator bodyText remainingBodyTexts = do
  case remainingBodyTexts of
    []
      | T.null bodyText ->
          Left $ NameError.unexpectedEnd m addressText "a name"
      | otherwise -> do
          bodySegments <- NP.normalize <$> mapM (BN.reflect m) (T.splitOn nsSep bodyText)
          case bodySegments of
            [] ->
              Left $ NameError.missingDefinition m addressText
            _ -> do
              localLocator <- LL.reflect m $ NP.reify bodySegments
              return Locator {modulePathText, sourceText, globalLocator, localLocator}
    _
      | T.null bodyText ->
          Left $ NameError.unexpectedSeparator m addressText doubleColon "a name"
      | otherwise ->
          Left $ NameError.unexpectedSeparator m addressText doubleColon "`.`"

reflectGlobalLocator :: H.Hint -> T.Text -> T.Text -> Either Error GL.GlobalLocator
reflectGlobalLocator m modulePathText sourceText = do
  GL.reflect m $ modulePathText <> doubleColon <> sourceText

reflectMaybe :: H.Hint -> T.Text -> Either Error (Maybe Locator)
reflectMaybe m addressText = do
  if doubleColon `T.isInfixOf` addressText
    then Just <$> reflect m addressText
    else return Nothing

reify :: Locator -> T.Text
reify Locator {globalLocator, localLocator} = do
  GL.reify globalLocator <> doubleColon <> LL.reify localLocator
