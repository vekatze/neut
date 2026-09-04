module Command.Archive.PackageVersion.PackageVersion
  ( PackageVersion,
    reflect,
    reify,
    isValidNewVersion,
    getAntecedents,
    getNewestVersion,
    compareVersion,
    increment,
    initialVersion,
  )
where

import Data.List qualified as List
import Data.Text qualified as T
import Kernel.Common.Const
import Language.Common.List (initLast)
import Text.Read

type PackageVersion =
  (AlphaPrefix, (MajorVersion, [MinorVersion]))

type AlphaPrefix =
  Int

type MajorVersion =
  Int

type MinorVersion =
  Int

reflect :: T.Text -> Maybe PackageVersion
reflect releaseName = do
  let intTextList = T.splitOn verSep releaseName
  intList <- mapM (readMaybe . T.unpack) intTextList
  let (zeroList, versionList) = span (== 0) intList
  (majorVersion, minorVersionList) <- List.uncons versionList
  if all (>= 0) $ majorVersion : minorVersionList
    then return (length zeroList, (majorVersion, minorVersionList))
    else Nothing

reify :: PackageVersion -> T.Text
reify version = do
  T.pack $ List.intercalate (T.unpack verSep) $ map show $ toComponentList version

toComponentList :: PackageVersion -> [Int]
toComponentList (alphaPrefix, (majorVersion, minorVersionList)) =
  replicate alphaPrefix 0 ++ (majorVersion : minorVersionList)

compareVersion :: PackageVersion -> PackageVersion -> Ordering
compareVersion v1 v2 =
  compare (toComponentList v1) (toComponentList v2)

isValidNewVersion :: PackageVersion -> [PackageVersion] -> Bool
isValidNewVersion (alphaPrefix, (majorVersion1, minorVersionList1)) vs = do
  let vs' = filter (\(a, (m, _)) -> a == alphaPrefix && m == majorVersion1) vs
  isValidNewVersion' minorVersionList1 $ map (snd . snd) vs'

isValidNewVersion' :: [MinorVersion] -> [[MinorVersion]] -> Bool
isValidNewVersion' newVersion vs =
  case vs of
    [] ->
      True
    existingVersion : rest ->
      existingVersion < newVersion && isValidNewVersion' newVersion rest

getAntecedents :: PackageVersion -> [PackageVersion] -> [PackageVersion]
getAntecedents (alphaPrefix, (majorVersion1, minorVersionList1)) vs = do
  let vs' = filter (\(a, (m, _)) -> a == alphaPrefix && m == majorVersion1) vs
  let antecedentMinorVersionList = filter (< minorVersionList1) $ map (snd . snd) vs'
  map ((alphaPrefix,) . (majorVersion1,)) antecedentMinorVersionList

getNewestVersion :: [PackageVersion] -> PackageVersion -> PackageVersion
getNewestVersion candidates fallback =
  List.foldl' newer fallback candidates

newer :: PackageVersion -> PackageVersion -> PackageVersion
newer v1 v2 =
  case compareVersion v1 v2 of
    LT ->
      v2
    _ ->
      v1

increment :: PackageVersion -> PackageVersion
increment version = do
  let (alphaPrefix, (majorVersion, minorVersionList)) = version
  case initLast minorVersionList of
    Just (minorVersionPrefix, lowestMinorVersion) ->
      (alphaPrefix, (majorVersion, minorVersionPrefix ++ [lowestMinorVersion + 1]))
    Nothing ->
      (alphaPrefix, (majorVersion + 1, []))

initialVersion :: PackageVersion
initialVersion =
  (1, (1, [0]))
