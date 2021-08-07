{-# LANGUAGE DeriveGeneric #-}

module Data.Stmt where

import Data.Basic
import Data.Binary
import qualified Data.ByteString.Lazy as L
import Data.Global
import Data.Term
import qualified Data.Text as T
import Data.WeakTerm
import GHC.Generics
import Path
import Path.IO

type HeaderStmtPlus =
  (Path Abs File, Either [Stmt] [WeakStmt])

type WeakStmtPlus =
  (Path Abs File, [WeakStmt])

-- WeakStmtDef Hint Ident WeakTermPlus WeakTermPlus
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

saveCache :: StmtPlus -> IO ()
saveCache (path, stmtList) = do
  let stmtList' = map compress stmtList
  let cacheData = encode stmtList'
  cachePath <- replaceExtension ".cache" path
  L.writeFile (toFilePath cachePath) cacheData

compress :: Stmt -> Stmt
compress stmt@(StmtDef isReducible m x t _) =
  if isReducible
    then stmt
    else StmtDef isReducible m x t (m, TermTau)

loadCache :: Path Abs File -> IO (Maybe [Stmt])
loadCache srcPath = do
  cachePath <- replaceExtension ".cache" srcPath
  hasCache <- doesFileExist cachePath
  -- fixme: hasCacheの判定にファイルの更新時刻を用いる
  if not hasCache
    then return Nothing
    else do
      srcModTime <- getModificationTime srcPath
      cacheModTime <- getModificationTime cachePath
      if cacheModTime < srcModTime
        then return Nothing
        else do
          cacheData <- L.readFile (toFilePath cachePath)
          let item = decode cacheData :: [Stmt]
          p' item
          return $ Just $ decode cacheData
