module Command.Describe.Describe
  ( Handle,
    new,
    describe,
  )
where

import App.App (App)
import App.Error (Error)
import App.Error qualified as Error
import App.Run (raiseError', tryApp)
import Command.Common.Build qualified as Build
import CommandParser.Config.Describe
import Control.Comonad.Cofree
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as B
import Data.Containers.ListUtils (nubOrd)
import Data.Either (fromRight, lefts)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sort)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Ens.Ens qualified as E
import Ens.ToDoc qualified as Ens
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Module qualified as M
import Kernel.Common.Source (sourceFilePath)
import Kernel.Common.Target
import Kernel.Unravel.Unravel qualified as Unravel
import Language.Common.ModuleDigest (ModuleDigest (..))
import Language.Common.ModuleID qualified as MID
import Logger.Hint (internalHint)
import Path
import Path.FileSystemEncoding (encodeFileSystemPath)
import SyntaxTree.Series qualified as SE
import System.IO (hFlush, stdout)

newtype Handle = Handle
  { globalHandle :: Global.Handle
  }

new :: Global.Handle -> Handle
new globalHandle = do
  Handle {..}

describe :: Handle -> Config -> App ()
describe h cfg = do
  target <- Build.getMainTarget (targetName cfg) (globalHandle h)
  unravelHandle <- liftIO $ Unravel.new (globalHandle h)
  reachedOrError <- liftIO $ tryApp $ reach h unravelHandle target
  writtenOrError <- liftIO $ tryApp $ writeDescription h unravelHandle target $ fromRight [] reachedOrError
  liftEither $ joinResults [void reachedOrError, writtenOrError]

reach :: Handle -> Unravel.Handle -> MainTarget -> App [M.Module]
reach h unravelHandle target = do
  mainModule <- Build.prepareMainModule (globalHandle h)
  result <- Unravel.unravel unravelHandle (M.extractModule mainModule) (Main target)
  let liveSourceSet = Unravel.resultLiveSourceSet result
  let liveSourceList = filter (flip S.member liveSourceSet . sourceFilePath) $ Unravel.resultSourceList result
  Unravel.getModuleList unravelHandle liveSourceList

writeDescription :: Handle -> Unravel.Handle -> MainTarget -> [M.Module] -> App ()
writeDescription h unravelHandle target moduleList = do
  let mainModule = M.extractModule $ Env.getMainModule $ Global.envHandle (globalHandle h)
  moduleRoot <- toUtf8Text $ toFilePath $ M.getModuleRootDir mainModule
  outputPath <- Path.getExecutableOutputPath (Global.pathHandle (globalHandle h)) target
  executable <- toUtf8Text $ toFilePath outputPath
  inputList <- liftIO (getInputList (globalHandle h) unravelHandle) >>= mapM (toUtf8Text . either toFilePath toFilePath)
  foreignList <- mapM (describeForeign h target) moduleList
  let fieldList =
        [ ("module-root", string moduleRoot),
          ("executable", string executable),
          ("input", list $ map string inputList),
          ("foreign", list foreignList)
        ]
  liftIO $ B.hPut stdout $ TE.encodeUtf8 $ Ens.pp $ E.inject $ E.dictFromListVertical internalHint fieldList
  liftIO $ hFlush stdout

describeForeign :: Handle -> MainTarget -> M.Module -> App E.Ens
describeForeign h target someModule = do
  let foreignInfo = M.moduleForeign someModule
  foreignDir <- Path.getForeignDir (Global.pathHandle (globalHandle h)) (Main target) someModule
  directory <- toUtf8Text $ toFilePath foreignDir
  outputList <- mapM (toUtf8Text . either toFilePath toFilePath) $ M.output foreignInfo
  linkList <- mapM (toUtf8Text . toFilePath) $ M.linkedOutput foreignInfo
  let digestField =
        case M.moduleID someModule of
          MID.Library (ModuleDigest digest) ->
            [("digest", string digest)]
          _ ->
            []
  return $
    E.dictFromListVertical internalHint $
      digestField
        ++ [ ("directory", string directory),
             ("output", list $ map string outputList),
             ("link", list $ map string linkList)
           ]

joinResults :: [Either Error ()] -> Either Error ()
joinResults results = do
  case lefts results of
    [] ->
      Right ()
    errors ->
      Left $ Error.join errors

string :: T.Text -> E.Ens
string t =
  internalHint :< E.String t

list :: [E.Ens] -> E.Ens
list xs =
  internalHint :< E.List ((SE.fromList SE.Bracket SE.Comma xs) {SE.hasOptionalSeparator = True})

toUtf8Text :: FilePath -> App T.Text
toUtf8Text path = do
  textOrNothing <- liftIO $ decodeUtf8Path path
  case textOrNothing of
    Just text ->
      return text
    Nothing ->
      raiseError' $ "A path in the description is not valid UTF-8: " <> T.pack path

decodeUtf8Path :: FilePath -> IO (Maybe T.Text)
decodeUtf8Path path = do
  bytesOrError <- try $ encodeFileSystemPath path
  case bytesOrError of
    Left (_ :: SomeException) ->
      return Nothing
    Right bytes ->
      return $ either (const Nothing) Just $ TE.decodeUtf8' bytes

getInputList :: Global.Handle -> Unravel.Handle -> IO [M.SomePath Abs]
getInputList h unravelHandle = do
  let mainModule = M.extractModule $ Env.getMainModule $ Global.envHandle h
  let moduleRootDir = M.getModuleRootDir mainModule
  visitedSourceList <- Unravel.getVisitedMainSourceList unravelHandle
  let staticFileList = map ((moduleRootDir </>) . snd) $ HashMap.elems $ M.moduleStaticFiles mainModule
  foreignInputList <-
    fmap concat $
      mapM (Path.unrollPath . M.attachPrefixPath moduleRootDir . snd) $
        M.input $
          M.moduleForeign mainModule
  return $
    sort $
      nubOrd $
        map Right (M.moduleLocation mainModule : visitedSourceList ++ staticFileList)
          ++ foreignInputList
