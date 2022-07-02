module Entity.Stmt where

import Control.Comonad.Cofree
import Data.Binary
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Binder
import Entity.EnumInfo
import Entity.Global
import Entity.Hint
import Entity.Opacity
import Entity.Source
import Entity.Term
import Entity.WeakTerm
import GHC.Generics
import GHC.IORef
import Path
import Path.IO

type WeakProgram =
  (Path Abs File, [WeakStmt])

data WeakStmt
  = WeakStmtDefine Opacity Hint T.Text Int [BinderF WeakTerm] WeakTerm WeakTerm
  | WeakStmtDefineResource Hint T.Text WeakTerm WeakTerm
  | WeakStmtSection Hint T.Text [WeakStmt]

data QuasiStmt
  = QuasiStmtDefine Opacity Hint T.Text Int [BinderF WeakTerm] WeakTerm WeakTerm
  | QuasiStmtDefineResource Hint T.Text WeakTerm WeakTerm

type Program =
  (Source, [Stmt])

data Stmt
  = StmtDefine Opacity Hint T.Text Int [BinderF Term] Term Term
  | StmtDefineResource Hint T.Text Term Term
  deriving (Generic)

instance Binary Stmt

extractName :: Stmt -> T.Text
extractName stmt = do
  case stmt of
    StmtDefine _ _ name _ _ _ _ ->
      name
    StmtDefineResource _ name _ _ ->
      name

data Cache = Cache
  { cacheStmtList :: [Stmt],
    cacheEnumInfo :: [EnumInfo]
  }
  deriving (Generic)

instance Binary Cache

compress :: Stmt -> Stmt
compress stmt =
  case stmt of
    StmtDefine opacity m functionName impArgNum args codType _ ->
      case opacity of
        OpacityOpaque ->
          StmtDefine opacity m functionName impArgNum args codType (m :< TermTau)
        _ ->
          stmt
    StmtDefineResource {} ->
      stmt

saveCache :: Program -> [EnumInfo] -> IO ()
saveCache (source, stmtList) enumInfoList = do
  b <- doesFreshCacheExist source
  if b
    then return ()
    else do
      cachePath <- getSourceCachePath source
      ensureDir $ parent cachePath
      encodeFile (toFilePath cachePath) (stmtList, enumInfoList)

loadCache :: Source -> IO (Maybe Cache)
loadCache source = do
  cachePath <- getSourceCachePath source
  hasCache <- doesFileExist cachePath
  if not hasCache
    then return Nothing
    else do
      b <- doesFreshCacheExist source
      if not b
        then return Nothing
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
  hasCacheSet <- readIORef hasCacheSetRef
  return $ S.member (sourceFilePath source) hasCacheSet
