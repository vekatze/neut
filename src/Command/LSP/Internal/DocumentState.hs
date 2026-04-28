module Command.LSP.Internal.DocumentState
  ( DocumentState (..),
    fromTexts,
    applyContentChange,
    baseParams,
    mapHighlights,
    mapLocation,
    mapDefinitionLink,
  )
where

import Control.Lens ((&), (.~), (^.))
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types

data Patch = Patch
  { baseStart :: !Int,
    baseEnd :: !Int,
    bufferStart :: !Int,
    bufferEnd :: !Int
  }

data DocumentState = DocumentState
  { baseText :: !T.Text,
    bufferText :: !T.Text,
    patches :: [Patch]
  }

commonPrefixLength :: T.Text -> T.Text -> Int
commonPrefixLength a b =
  case T.commonPrefixes a b of
    Just (p, _, _) -> T.length p
    Nothing -> 0

commonSuffixLength :: T.Text -> T.Text -> Int
commonSuffixLength a b = do
  commonPrefixLength (T.reverse a) (T.reverse b)

shiftBuffer :: Int -> Patch -> Patch
shiftBuffer d patch =
  patch {bufferStart = bufferStart patch + d, bufferEnd = bufferEnd patch + d}

pruneNoOp :: T.Text -> T.Text -> Patch -> Maybe Patch
pruneNoOp baseText bufferText patch = do
  let baseSegment = T.take (baseEnd patch - baseStart patch) (T.drop (baseStart patch) baseText)
  let bufferSegment = T.take (bufferEnd patch - bufferStart patch) (T.drop (bufferStart patch) bufferText)
  if baseSegment == bufferSegment
    then Nothing
    else Just patch

convertForward :: [Patch] -> Int -> Maybe Int
convertForward mappings baseIndex =
  go 0 0 mappings
  where
    go baseCursor bufferCursor xs =
      case xs of
        [] ->
          Just (bufferCursor + baseIndex - baseCursor)
        patch : rest
          | baseIndex <= baseStart patch ->
              Just (bufferCursor + baseIndex - baseCursor)
          | baseIndex < baseEnd patch ->
              Nothing
          | otherwise ->
              go (baseEnd patch) (bufferEnd patch) rest

convertBackward :: [Patch] -> Int -> Maybe Int
convertBackward mappings bufferIndex =
  go 0 0 mappings
  where
    go baseCursor bufferCursor xs =
      case xs of
        [] ->
          Just (baseCursor + bufferIndex - bufferCursor)
        patch : rest
          | bufferIndex <= bufferStart patch ->
              Just (baseCursor + bufferIndex - bufferCursor)
          | bufferIndex < bufferEnd patch ->
              Nothing
          | otherwise ->
              go (baseEnd patch) (bufferEnd patch) rest

isStable :: [Patch] -> Int -> Int -> Bool
isStable patches lo hi =
  all stable patches
  where
    stable patch
      | baseStart patch == baseEnd patch = do
          let p = baseStart patch
          p <= lo || hi <= p
      | otherwise =
          baseEnd patch <= lo || hi <= baseStart patch

positionToIndex :: T.Text -> Position -> Maybe Int
positionToIndex text pos =
  go 0 0 text
  where
    targetLine = fromIntegral (pos ^. J.line) :: Int
    targetChar = fromIntegral (pos ^. J.character) :: Int
    go index lineNo remaining
      | lineNo == targetLine = do
          let lineLen =
                case T.findIndex (== '\n') remaining of
                  Just i ->
                    i
                  Nothing ->
                    T.length remaining
          if targetChar <= lineLen
            then Just (index + targetChar)
            else Nothing
      | otherwise =
          case T.findIndex (== '\n') remaining of
            Just i ->
              go (index + i + 1) (lineNo + 1) (T.drop (i + 1) remaining)
            Nothing ->
              Nothing

indexToPosition :: T.Text -> Int -> Maybe Position
indexToPosition text targetIndex
  | targetIndex < 0 || targetIndex > T.length text =
      Nothing
  | otherwise =
      Just $ go 0 0 text
  where
    go :: Int -> Int -> T.Text -> Position
    go lineStart lineNo remaining =
      case T.findIndex (== '\n') remaining of
        Just i
          | targetIndex <= lineStart + i ->
              Position (fromIntegral lineNo) (fromIntegral (targetIndex - lineStart))
          | otherwise ->
              go (lineStart + i + 1) (lineNo + 1) (T.drop (i + 1) remaining)
        Nothing ->
          Position (fromIntegral lineNo) (fromIntegral (targetIndex - lineStart))

mergeIncomingEdit :: DocumentState -> Int -> Int -> T.Text -> Maybe DocumentState
mergeIncomingEdit o editRegionStart editRegionEnd newChunk = do
  let newBufferText = T.take editRegionStart (bufferText o) <> newChunk <> T.drop editRegionEnd (bufferText o)
  let (before, rest) = span (\patch -> bufferEnd patch < editRegionStart) (patches o)
  let (touched, after) = span (\patch -> bufferStart patch <= editRegionEnd) rest
  let minBufferStart = minimum $ editRegionStart : map bufferStart touched
  let maxBufferEnd = maximum $ editRegionEnd : map bufferEnd touched
  let delta = T.length newChunk - (editRegionEnd - editRegionStart)
  mergedBaseStart <- convertBackward (patches o) minBufferStart
  mergedBaseEnd <- convertBackward (patches o) maxBufferEnd
  let shiftedAfter = map (shiftBuffer delta) after
  let mergedEdit =
        Patch
          { baseStart = mergedBaseStart,
            baseEnd = mergedBaseEnd,
            bufferStart = minBufferStart,
            bufferEnd = maxBufferEnd + delta
          }
  let newPatches = before ++ maybeToList (pruneNoOp (baseText o) newBufferText mergedEdit) ++ shiftedAfter
  return o {bufferText = newBufferText, patches = newPatches}

singleRegionDiff :: T.Text -> T.Text -> [Patch]
singleRegionDiff a b = do
  let p = commonPrefixLength a b
  let s = commonSuffixLength (T.drop p a) (T.drop p b)
  [ Patch
      { baseStart = p,
        baseEnd = T.length a - s,
        bufferStart = p,
        bufferEnd = T.length b - s
      }
    ]

fromTexts :: T.Text -> T.Text -> DocumentState
fromTexts baseText bufferText =
  DocumentState {baseText, bufferText, patches = singleRegionDiff baseText bufferText}

applyContentChange :: DocumentState -> TextDocumentContentChangeEvent -> Maybe DocumentState
applyContentChange o (TextDocumentContentChangeEvent change) =
  case change of
    InL TextDocumentContentChangePartial {_range, _text = replacement} -> do
      l <- positionToIndex (bufferText o) (_range ^. J.start)
      r <- positionToIndex (bufferText o) (_range ^. J.end)
      if l > r
        then Nothing
        else mergeIncomingEdit o l r replacement
    InR TextDocumentContentChangeWholeDocument {_text = newBufferText} ->
      Just $ fromTexts (baseText o) newBufferText

bufferToBasePosition :: DocumentState -> Position -> Maybe Position
bufferToBasePosition o pos = do
  index <- positionToIndex (bufferText o) pos
  baseIndex <- convertBackward (patches o) index
  indexToPosition (baseText o) baseIndex

baseParams :: (J.HasPosition p Position) => Maybe DocumentState -> p -> Maybe p
baseParams documentState params =
  case documentState of
    Nothing ->
      Just params
    Just ds -> do
      pos' <- bufferToBasePosition ds (params ^. J.position)
      return $ params & J.position .~ pos'

baseToBufferRange :: DocumentState -> Range -> Maybe Range
baseToBufferRange o (Range s e) = do
  startIndex <- positionToIndex (baseText o) s
  endIndex <- positionToIndex (baseText o) e
  if not (isStable (patches o) startIndex endIndex)
    then Nothing
    else do
      bufferEndIndex <- convertForward (patches o) endIndex
      let bufferStartIndex = bufferEndIndex - (endIndex - startIndex)
      newStart <- indexToPosition (bufferText o) bufferStartIndex
      newEnd <- indexToPosition (bufferText o) bufferEndIndex
      return $ Range newStart newEnd

rangeContainsPos :: Range -> Position -> Bool
rangeContainsPos (Range s e) p =
  s <= p && p <= e

mapHighlights :: Maybe DocumentState -> Position -> [DocumentHighlight] -> [DocumentHighlight]
mapHighlights documentState cursor highlights = do
  let inBuffer =
        case documentState of
          Nothing ->
            highlights
          Just ds ->
            flip mapMaybe highlights $ \DocumentHighlight {_range, _kind} -> do
              r <- baseToBufferRange ds _range
              return DocumentHighlight {_range = r, _kind}
  if any (\hl -> rangeContainsPos (hl ^. J.range) cursor) inBuffer
    then inBuffer
    else []

mapLocation :: Maybe DocumentState -> Location -> Maybe Location
mapLocation documentState loc@Location {_uri, _range} =
  case documentState of
    Nothing ->
      Just loc
    Just ds -> do
      r <- baseToBufferRange ds _range
      return Location {_uri, _range = r}

mapDefinitionLink :: Maybe DocumentState -> DefinitionLink -> Maybe DefinitionLink
mapDefinitionLink documentState link = do
  let DefinitionLink ll@LocationLink {_targetUri, _targetRange, _targetSelectionRange} = link
  case documentState of
    Nothing ->
      Just $ DefinitionLink ll
    Just ds -> do
      tr <- baseToBufferRange ds _targetRange
      tsr <- baseToBufferRange ds _targetSelectionRange
      return $ DefinitionLink $ ll {_targetRange = tr, _targetSelectionRange = tsr}
