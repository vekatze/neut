{-# LANGUAGE DeriveGeneric #-}

module Data.Stmt where

import Data.Basic
import Data.Binary
import Data.Global
import Data.Term
import qualified Data.Text as T
import Data.WeakTerm
import GHC.Generics
import Path
import Path.IO

type HeaderStmtPlus =
  (Path Abs File, Either [Stmt] [WeakStmt]) -- [EnumInfo]

type WeakStmtPlus =
  (Path Abs File, [WeakStmt])

data WeakStmt
  = WeakStmtDef IsReducible Hint T.Text WeakTermPlus WeakTermPlus
  | WeakStmtUse T.Text
  | WeakStmtUnuse T.Text
  | WeakStmtDefinePrefix T.Text T.Text
  | WeakStmtRemovePrefix T.Text T.Text
  deriving (Show)

type StmtPlus =
  (Path Abs File, [Stmt])

data Stmt
  = StmtDef IsReducible Hint T.Text TermPlus TermPlus
  deriving (Show, Generic)

instance Binary Stmt

type EnumInfo = (Hint, T.Text, [(T.Text, Int)])

data Cache = Cache
  { cacheSrcPath :: Path Abs File,
    cacheDefList :: [Stmt],
    cacheEnumList :: [EnumInfo]
  }

saveCache :: StmtPlus -> IO ()
saveCache (path, stmtList) = do
  let stmtList' = map compress stmtList
  cachePath <- replaceExtension ".cache" path
  encodeFile (toFilePath cachePath) stmtList'

compress :: Stmt -> Stmt
compress stmt@(StmtDef isReducible m x t _) =
  if isReducible
    then stmt
    else StmtDef isReducible m x t (m, TermTau)

loadCache :: Hint -> Path Abs File -> IO (Maybe [Stmt])
loadCache m srcPath = do
  cachePath <- replaceExtension ".cache" srcPath
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return Nothing
    else do
      srcModTime <- getModificationTime srcPath
      cacheModTime <- getModificationTime cachePath
      if cacheModTime < srcModTime
        then return Nothing
        else do
          dataOrErr <- decodeFileOrFail (toFilePath cachePath)
          case dataOrErr of
            Left _ -> do
              warn (getPosInfo m) $ "the cache file `" <> T.pack (toFilePath cachePath) <> "` is malformed, and thus is removed."
              removeFile cachePath
              return Nothing
            Right content ->
              return $ Just content
