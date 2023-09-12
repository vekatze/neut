module Scene.Parse.Import
  ( procImportStmt,
    interpretImportTree,
  )
where

import Context.Alias qualified as Alias
import Context.App
import Context.Global qualified as Global
import Context.NameDependence qualified as NameDependence
import Context.Throw (liftEither)
import Context.Throw qualified as Throw
import Context.Via qualified as Via
import Control.Comonad.Cofree
import Control.Monad
import Data.Bifunctor (second)
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.AliasInfo qualified as AI
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.GlobalLocator qualified as GL
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Entity.Module
import Entity.Source qualified as Source
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Tree
import Path
import Scene.Module.Reflect qualified as Module
import Scene.Source.ShiftToLatest

type PosText = (Hint, T.Text)

type LocatorText =
  T.Text

procImportStmt :: Source.Source -> Tree -> App ()
procImportStmt currentSource importTree@(m :< _) = do
  sourceInfoList <- interpretImportTree currentSource importTree
  activateNameInfo m sourceInfoList

interpretImportTree :: Source.Source -> Tree -> App [(Source.Source, [AI.AliasInfo])]
interpretImportTree currentSource importTree = do
  (m, locatorTrees) <- liftEither $ access' "import" importTree
  locInfoList <- mapM getLocatorTexts locatorTrees
  locatorAndSourceInfo <- mapM interpretImport locInfoList
  let (foundLocators, sourceInfo) = unzip locatorAndSourceInfo
  coreSourceInfo <- loadDefaultImports m currentSource foundLocators
  return $ sourceInfo ++ coreSourceInfo

activateNameInfo :: Hint -> [(Source.Source, [AI.AliasInfo])] -> App ()
activateNameInfo m sourceInfoList = do
  forM_ sourceInfoList $ \(source, aliasInfoList) -> do
    let path = Source.sourceFilePath source
    namesInSource <- Global.lookupSourceNameMap m path
    Global.activateTopLevelNames namesInSource
    forM_ aliasInfoList $ \aliasInfo ->
      Alias.activateAliasInfo namesInSource aliasInfo
    NameDependence.get path >>= Global.activateTopLevelNames
    Via.get path >>= Via.addToActiveViaMap

getLocatorTexts :: Tree -> App (PosText, [PosText])
getLocatorTexts t =
  case t of
    _ :< Node [atom, xs] -> do
      atom' <- liftEither $ getSymbol atom
      (_, xs') <- liftEither $ toList1 xs
      xs'' <- mapM (liftEither . getSymbol) xs'
      return (atom', xs'')
    m :< _ ->
      Throw.raiseError m "getLocatorTexts"

interpretImport ::
  (PosText, [PosText]) ->
  App (LocatorText, (Source.Source, [AI.AliasInfo]))
interpretImport ((m, locatorText), nameList) = do
  (source, strictGlobalLocator) <- parseLocatorText m locatorText
  nameList' <- mapM (liftEither . BN.fromTextOptional) nameList
  let localLocators = map (second LL.new) nameList'
  let useInfo = AI.Use strictGlobalLocator localLocators
  return (locatorText, (source, [useInfo]))

parseLocatorText :: Hint -> T.Text -> App (Source.Source, SGL.StrictGlobalLocator)
parseLocatorText m locatorText = do
  (moduleAlias, sourceLocator) <- Throw.liftEither $ GL.reflectLocator m locatorText
  strictGlobalLocator <- Alias.resolveLocatorAlias m moduleAlias sourceLocator
  source <- getSource m strictGlobalLocator locatorText >>= shiftToLatest
  return (source, strictGlobalLocator)

getSource :: Hint -> SGL.StrictGlobalLocator -> LocatorText -> App Source.Source
getSource m sgl locatorText = do
  nextModule <- Module.getModule m (SGL.moduleID sgl) locatorText
  relPath <- addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  return $
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = getSourceDir nextModule </> relPath,
        Source.sourceHint = Just m
      }

loadDefaultImports :: Hint -> Source.Source -> [LocatorText] -> App [(Source.Source, [AI.AliasInfo])]
loadDefaultImports m source foundLocators =
  if not (Source.hasCore source)
    then return []
    else do
      let locatorSet = S.fromList foundLocators
      let defaultImports' = filter (\(x, _) -> S.notMember x locatorSet) BN.defaultImports
      mapM (uncurry $ interpretDefaultImport m) defaultImports'

interpretDefaultImport :: Hint -> T.Text -> [BN.BaseName] -> App (Source.Source, [AI.AliasInfo])
interpretDefaultImport m globalLocatorText localLocatorBaseNameList = do
  (source, sgl) <- parseLocatorText m globalLocatorText
  let lls = map LL.new localLocatorBaseNameList
  return (source, [AI.Use sgl (map (m,) lls)])
