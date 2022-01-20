{-# LANGUAGE DeriveGeneric #-}

module Data.Stmt where

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Basic (BinderF, Hint, Opacity (OpacityOpaque))
import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import Data.Global (modifiedSourceSetRef, topNameSetRef)
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
  = WeakStmtDefine Opacity Hint T.Text [BinderF WeakTerm] WeakTerm WeakTerm
  | WeakStmtDefineResource Hint T.Text WeakTerm WeakTerm

type Program =
  (Source, [Stmt])

data Stmt
  = StmtDefine Opacity Hint T.Text [BinderF Term] Term Term
  | StmtDefineResource Hint T.Text Term Term
  deriving (Generic)

instance Binary Stmt

extractName :: Stmt -> T.Text
extractName stmt = do
  case stmt of
    StmtDefine _ _ name _ _ _ ->
      name
    StmtDefineResource _ name _ _ ->
      name

type EnumInfo = (Hint, T.Text, [(T.Text, Int)])

type Cache = ([Stmt], [EnumInfo])

compress :: Stmt -> Stmt
compress stmt =
  case stmt of
    StmtDefine opacity m functionName args codType _ ->
      case opacity of
        OpacityOpaque ->
          StmtDefine opacity m functionName args codType (m :< TermTau)
        _ ->
          stmt
    StmtDefineResource m name _ _ ->
      StmtDefineResource m name (m :< TermTau) (m :< TermTau)

isPublic :: S.Set T.Text -> Stmt -> Bool
isPublic topNameSet stmt =
  S.member (extractName stmt) topNameSet

saveCache :: Program -> [EnumInfo] -> IO ()
saveCache (source, stmtList) enumInfoList = do
  b <- doesFreshCacheExist source
  if b
    then return () -- the cache is already fresh
    else do
      topNameSet <- readIORef topNameSetRef
      let stmtList' = map compress $ filter (isPublic topNameSet) stmtList
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
