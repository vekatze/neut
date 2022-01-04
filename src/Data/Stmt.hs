{-# LANGUAGE DeriveGeneric #-}

module Data.Stmt where

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Basic (Hint, Opacity (OpacityOpaque))
import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import Data.Global (modifiedSourceSetRef)
import qualified Data.Set as S
import Data.Source (Source (sourceFilePath), getSourceCachePath)
import Data.Term (Term, TermF (..))
import qualified Data.Text as T
import Data.WeakTerm (WeakTerm)
import GHC.Generics (Generic)
import GHC.IORef (readIORef)
import Path (Abs, File, Path, parent, toFilePath)
import Path.IO (doesFileExist, ensureDir, removeFile)

type WeakProgram =
  (Path Abs File, [WeakStmt])

data WeakStmt
  = WeakStmtDefine Opacity Hint T.Text WeakTerm WeakTerm

type Program =
  (Source, [Stmt])

data Stmt
  = StmtDefine Opacity Hint T.Text Term Term
  deriving (Generic)

instance Binary Stmt

type EnumInfo = (Hint, T.Text, [(T.Text, Int)])

type Cache = ([Stmt], [EnumInfo])

compress :: Stmt -> Stmt
compress stmt@(StmtDefine opacity m x t _) =
  case opacity of
    OpacityOpaque ->
      StmtDefine opacity m x t (m :< TermTau)
    _ ->
      stmt

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

loadCache :: Source -> IO (Maybe Cache)
loadCache source = do
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
              removeFile cachePath
              return Nothing
            Right content ->
              return $ Just content

doesFreshCacheExist :: Source -> IO Bool
doesFreshCacheExist source = do
  modifiedSourceSet <- readIORef modifiedSourceSetRef
  return $ S.notMember (sourceFilePath source) modifiedSourceSet
