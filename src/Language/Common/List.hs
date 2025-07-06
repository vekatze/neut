module Language.Common.List (initLast) where

initLast :: [a] -> Maybe ([a], a)
initLast xs =
  case xs of
    [] ->
      Nothing
    [x] ->
      return ([], x)
    x : rest -> do
      (initElems, lastElem) <- initLast rest
      return (x : initElems, lastElem)
