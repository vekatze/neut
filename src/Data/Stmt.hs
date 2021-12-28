{-# LANGUAGE DeriveGeneric #-}

module Data.Stmt where

import Data.Basic (Hint, IsReducible, getPosInfo)
import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import Data.Global (warn)
import Data.Source (Source (sourceFilePath), getSourceCachePath)
import Data.Term (Term)
import qualified Data.Text as T
import Data.WeakTerm (WeakTerm)
import GHC.Generics (Generic)
import Path (Abs, File, Path, parent, toFilePath)
import Path.IO (doesFileExist, ensureDir, getModificationTime, removeFile)

type HeaderProgram =
  (Path Abs File, Either [Stmt] [WeakStmt], [EnumInfo])

type WeakProgram =
  (Path Abs File, [WeakStmt])

data WeakStmt
  = WeakStmtDef IsReducible Hint T.Text WeakTerm WeakTerm

type Program =
  (Source, [Stmt])

data Stmt
  = StmtDef IsReducible Hint T.Text Term Term
  deriving (Generic)

instance Binary Stmt

type EnumInfo = (Hint, T.Text, [(T.Text, Int)])

type Cache = ([Stmt], [EnumInfo])

compress :: Stmt -> Stmt
compress = id

-- compress stmt@(StmtDef isReducible m x t _) =
--   if isReducible
--     then stmt
--     else StmtDef isReducible m x t (m :< TermTau)

saveCache :: Program -> [EnumInfo] -> IO ()
saveCache (source, stmtList) enumInfoList = do
  b <- doesFreshCacheExist source
  if b
    then return () -- the cache is already fresh
    else do
      let stmtList' = map compress stmtList
      cachePath <- getSourceCachePath source
      ensureDir $ parent cachePath
      encodeFile (toFilePath cachePath) (stmtList', enumInfoList)

loadCache :: Hint -> Source -> IO (Maybe Cache)
loadCache m source = do
  cachePath <- getSourceCachePath source
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return Nothing
    else do
      b <- doesFreshCacheExist source
      if not b
        then return Nothing -- no cache is available
        else do
          dataOrErr <- decodeFileOrFail (toFilePath cachePath)
          case dataOrErr of
            Left _ -> do
              warn (getPosInfo m) $ "the cache file `" <> T.pack (toFilePath cachePath) <> "` is malformed, and thus is removed."
              removeFile cachePath
              return Nothing
            Right content ->
              return $ Just content

doesFreshCacheExist :: Source -> IO Bool
doesFreshCacheExist source = do
  cachePath <- getSourceCachePath source
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return False
    else do
      srcModTime <- getModificationTime $ sourceFilePath source
      cacheModTime <- getModificationTime cachePath
      return $ cacheModTime > srcModTime
