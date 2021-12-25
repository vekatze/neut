{-# LANGUAGE DeriveGeneric #-}

module Data.Stmt where

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Basic (Hint, IsReducible, getPosInfo)
import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import Data.Global (p, warn)
import Data.Term (Term, TermF (TermTau))
import qualified Data.Text as T
import Data.WeakTerm (WeakTerm)
import GHC.Generics (Generic)
import Path (Abs, File, Path, replaceExtension, toFilePath)
import Path.IO (doesFileExist, getModificationTime, removeFile)

type HeaderProgram =
  (Path Abs File, Either [Stmt] [WeakStmt], [EnumInfo])

type WeakProgram =
  (Path Abs File, [WeakStmt])

data WeakStmt
  = WeakStmtDef IsReducible Hint T.Text WeakTerm WeakTerm

type Program =
  (Path Abs File, [Stmt])

data Stmt
  = StmtDef IsReducible Hint T.Text Term Term
  deriving (Generic)

instance Binary Stmt

type EnumInfo = (Hint, T.Text, [(T.Text, Int)])

type Cache = ([Stmt], [EnumInfo])

compress :: Stmt -> Stmt
compress stmt@(StmtDef isReducible m x t _) =
  if isReducible
    then stmt
    else StmtDef isReducible m x t (m :< TermTau)

saveCache :: Program -> [EnumInfo] -> IO ()
saveCache (srcPath, stmtList) enumInfoList = do
  b <- doesFreshCacheExist srcPath
  if b
    then return () -- the cache is already fresh
    else do
      let stmtList' = map compress stmtList
      cachePath <- replaceExtension ".cache" srcPath
      encodeFile (toFilePath cachePath) (stmtList', enumInfoList)

loadCache :: Hint -> Path Abs File -> IO (Maybe Cache)
loadCache m srcPath = do
  cachePath <- replaceExtension ".cache" srcPath
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return Nothing
    else do
      b <- doesFreshCacheExist srcPath
      if not b
        then do
          p "no cache is available"
          return Nothing -- no cache is available
        else do
          dataOrErr <- decodeFileOrFail (toFilePath cachePath)
          case dataOrErr of
            Left _ -> do
              warn (getPosInfo m) $ "the cache file `" <> T.pack (toFilePath cachePath) <> "` is malformed, and thus is removed."
              removeFile cachePath
              return Nothing
            Right content ->
              return $ Just content

doesFreshCacheExist :: Path Abs File -> IO Bool
doesFreshCacheExist srcPath = do
  cachePath <- replaceExtension ".cache" srcPath
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return False
    else do
      srcModTime <- getModificationTime srcPath
      cacheModTime <- getModificationTime cachePath
      return $ cacheModTime > srcModTime
