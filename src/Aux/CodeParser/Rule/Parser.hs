module Aux.CodeParser.Rule.Parser
  ( Parser,
    createParseError,
    nonSymbolCharSet,
    asTokens,
    asLabel,
  )
where

import Aux.Error.Rule.EIO
import Aux.Error.Rule.Error qualified as E
import Aux.Logger.Rule.Hint (SavedHint (SavedHint))
import Aux.Logger.Rule.Hint.Reflect (fromSourcePos)
import Aux.Logger.Rule.Log (Log (..))
import Aux.Logger.Rule.LogLevel (LogLevel (Error))
import Data.List.NonEmpty
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec

type Parser a = ParsecT Void T.Text EIO a

createParseError :: ParseErrorBundle T.Text Void -> E.Error
createParseError errorBundle = do
  let (foo, posState) = attachSourcePos errorOffset (bundleErrors errorBundle) (bundlePosState errorBundle)
  let pos = pstateSourcePos posState
  let m = fromSourcePos pos
  E.MakeError
    [ Log
        { position = Just (SavedHint m),
          shouldInsertPadding = True,
          logLevel = Error,
          content = T.pack $ concatMap (parseErrorTextPretty . fst) $ toList foo
        }
    ]

{-# INLINE nonSymbolCharSet #-}
nonSymbolCharSet :: S.Set Char
nonSymbolCharSet =
  S.fromList "=() `\"\n\t:;,<>[]{}/*|"

asTokens :: T.Text -> ErrorItem Char
asTokens s =
  Tokens $ fromList $ T.unpack s

asLabel :: T.Text -> ErrorItem Char
asLabel s =
  Label $ fromList $ T.unpack s
