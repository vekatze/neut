module Kernel.Common.PlatformPattern
  ( PlatformPattern,
    reflect,
    reify,
    sameHostAndTarget,
    matches,
    matchesHost,
    matchesSomePlatform,
    determinesTarget,
    resolveTarget,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text qualified as T
import Kernel.Common.Platform qualified as P

data Segment
  = Literal T.Text
  | Wildcard
  | Variable T.Text
  deriving (Eq, Show)

data PlatformPattern = PlatformPattern
  { _hostPart :: [Segment],
    _targetPart :: [Segment]
  }
  deriving (Eq, Show)

type Binding =
  Map.HashMap T.Text T.Text

reify :: PlatformPattern -> T.Text
reify platformPattern = do
  let reifyPart = T.concat . map reifySegment
  reifyPart (_hostPart platformPattern) <> "/" <> reifyPart (_targetPart platformPattern)

reifySegment :: Segment -> T.Text
reifySegment segment =
  case segment of
    Literal literal ->
      literal
    Wildcard ->
      "*"
    Variable name ->
      "${" <> name <> "}"

reflect :: T.Text -> Maybe PlatformPattern
reflect text =
  case T.splitOn "/" text of
    [hostText, targetText] -> do
      hostPart <- parseSegmentList hostText
      targetPart <- parseSegmentList targetText
      return $ PlatformPattern {_hostPart = hostPart, _targetPart = targetPart}
    _ ->
      Nothing

sameHostAndTarget :: PlatformPattern
sameHostAndTarget =
  PlatformPattern
    { _hostPart = [Variable "0"],
      _targetPart = [Variable "0"]
    }

parseSegmentList :: T.Text -> Maybe [Segment]
parseSegmentList input =
  case T.uncons input of
    Nothing ->
      return []
    Just ('*', rest) ->
      (Wildcard :) <$> parseSegmentList rest
    Just ('$', rest) -> do
      body <- T.stripPrefix "{" rest
      let (name, closing) = T.breakOn "}" body
      rest' <- T.stripPrefix "}" closing
      if T.null name
        then Nothing
        else (Variable name :) <$> parseSegmentList rest'
    Just _ -> do
      let (literal, rest) = T.break isSegmentStart input
      (Literal literal :) <$> parseSegmentList rest

isSegmentStart :: Char -> Bool
isSegmentStart c =
  c == '*' || c == '$'

matchSegmentList :: [Segment] -> T.Text -> Binding -> [Binding]
matchSegmentList segmentList input binding = do
  let matchAfterPrefix prefix rest = do
        remaining <- maybeToList (T.stripPrefix prefix input)
        matchSegmentList rest remaining binding
  case segmentList of
    [] ->
      [binding | T.null input]
    Literal literal : rest ->
      matchAfterPrefix literal rest
    Wildcard : rest -> do
      (_, remaining) <- splitLongestFirst input
      matchSegmentList rest remaining binding
    Variable name : rest ->
      case Map.lookup name binding of
        Just value ->
          matchAfterPrefix value rest
        Nothing -> do
          (value, remaining) <- splitLongestFirst input
          matchSegmentList rest remaining (Map.insert name value binding)

splitLongestFirst :: T.Text -> [(T.Text, T.Text)]
splitLongestFirst input =
  map (`T.splitAt` input) [T.length input, T.length input - 1 .. 0]

hostBindings :: PlatformPattern -> P.PlatformName -> [Binding]
hostBindings platformPattern host =
  matchSegmentList (_hostPart platformPattern) (P.reifyName host) Map.empty

matches :: PlatformPattern -> P.Platform -> Bool
matches platformPattern platform = do
  let targetText = P.reifyName (P.target platform)
  let matchesTarget binding =
        not $ null $ matchSegmentList (_targetPart platformPattern) targetText binding
  any matchesTarget (hostBindings platformPattern (P.host platform))

matchesHost :: PlatformPattern -> P.PlatformName -> Bool
matchesHost platformPattern host =
  not $ null $ hostBindings platformPattern host

matchesSomePlatform :: PlatformPattern -> Bool
matchesSomePlatform platformPattern =
  any (matches platformPattern) P.allPlatforms

determinesTarget :: PlatformPattern -> Bool
determinesTarget platformPattern = do
  let hostVariables = [name | Variable name <- _hostPart platformPattern]
  all (isDeterminedBy hostVariables) (_targetPart platformPattern)

isDeterminedBy :: [T.Text] -> Segment -> Bool
isDeterminedBy hostVariables segment =
  case segment of
    Literal _ ->
      True
    Wildcard ->
      False
    Variable name ->
      name `elem` hostVariables

resolveTarget :: PlatformPattern -> P.PlatformName -> Maybe P.PlatformName
resolveTarget platformPattern host = do
  binding <- listToMaybe (hostBindings platformPattern host)
  targetText <- T.concat <$> mapM (instantiate binding) (_targetPart platformPattern)
  P.reflectName targetText

instantiate :: Binding -> Segment -> Maybe T.Text
instantiate binding segment =
  case segment of
    Literal literal ->
      return literal
    Wildcard ->
      Nothing
    Variable name ->
      Map.lookup name binding
