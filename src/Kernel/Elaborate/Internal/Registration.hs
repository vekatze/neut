{-# LANGUAGE DerivingVia #-}

module Kernel.Elaborate.Internal.Registration
  ( Registration (..),
    register,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Set qualified as S
import GHC.Generics (Generic, Generically (..))
import Kernel.Common.Arch qualified as Arch
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Data qualified as Data
import Kernel.Common.Handle.Global.Expose qualified as Expose
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Handle.Global.Resource qualified as Resource
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Common.Platform qualified as P
import Kernel.Elaborate.Internal.Handle.Def qualified as Definition
import Kernel.Elaborate.Internal.Handle.Elaborate
import Kernel.Elaborate.Internal.Handle.Trope qualified as Trope
import Kernel.Elaborate.Internal.Handle.TypeDef qualified as TypeDef
import Kernel.Elaborate.Internal.Handle.WeakDecl qualified as WeakDecl
import Kernel.Elaborate.Internal.Handle.WeakDef qualified as WeakDef
import Kernel.Elaborate.Internal.Handle.WeakTypeDef qualified as WeakTypeDef
import Language.Common.Binder
import Language.Common.DataInfo qualified as DI
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.Foreign qualified as F
import Language.LowComp.DeclarationName qualified as DN
import Language.Term.Inline.Handle qualified as InlineHandle
import Language.Term.Stmt qualified as Stmt
import Language.Term.Term qualified as TM
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint

data Registration = Registration
  { typeEntries :: [(DD.DefiniteDescription, WT.WeakType)],
    definitionEntries :: [(DD.DefiniteDescription, InlineHandle.DefInfo)],
    typeDefEntries :: [(DD.DefiniteDescription, TypeDef.TypeDefInfo)],
    dataInfoEntries :: [(DD.DefiniteDescription, DI.DataInfo (BinderF TM.Type))],
    resourceEntries :: [(DD.DefiniteDescription, Resource.ResourceLayout)],
    tropeEntries :: [(DD.DefiniteDescription, [Stmt.DefineMeta])],
    exposeEntries :: [[(Hint, DD.DefiniteDescription, EN.ExternalName)]],
    weakDefEntries :: [WeakDef.Entry],
    weakTypeDefEntries :: [(DD.DefiniteDescription, WeakTypeDef.TypeDef)],
    weakDataInfoEntries :: [(DD.DefiniteDescription, DI.DataInfo (BinderF WT.WeakType))],
    foreignEntries :: [(DN.DeclarationName, F.ForeignSignature WT.WeakType)]
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via (Generically Registration)

register :: Handle -> Registration -> App ()
register h registration = do
  mapM_ (ensureExposedNameLinearity S.empty) $ exposeEntries registration
  liftIO $ Type.insertMany (typeHandle h) $ typeEntries registration
  liftIO $ Definition.insertMany (defHandle h) $ definitionEntries registration
  liftIO $ Data.insertMany (dataHandle h) $ dataInfoEntries registration
  liftIO $ Resource.insertMany (Global.resourceHandle $ globalHandle h) $ resourceEntries registration
  liftIO $ TypeDef.insertMany (typeDefHandle h) $ typeDefEntries registration
  liftIO $ Trope.insertMany (tropeHandle h) $ tropeEntries registration
  forM_ (concat $ exposeEntries registration) $ \(m, dd, extName) -> registerExpose h m dd extName
  liftIO $ WeakDef.insertMany (weakDefHandle h) (gensymHandle h) $ weakDefEntries registration
  liftIO $ Data.insertWeakMany (dataHandle h) $ weakDataInfoEntries registration
  liftIO $ WeakTypeDef.insertMany (weakTypeDefHandle h) $ weakTypeDefEntries registration
  liftIO $ forM_ (foreignEntries registration) $ uncurry (WeakDecl.insert (weakDeclHandle h))

ensureExposedNameLinearity :: S.Set EN.ExternalName -> [(Hint, DD.DefiniteDescription, EN.ExternalName)] -> App ()
ensureExposedNameLinearity found exportList =
  case exportList of
    [] ->
      return ()
    (m, _, extName) : rest
      | S.member extName found ->
          raiseError m $ "`" <> EN.reify extName <> "` is already exposed"
      | otherwise ->
          ensureExposedNameLinearity (S.insert extName found) rest

registerExpose :: Handle -> Hint -> DD.DefiniteDescription -> EN.ExternalName -> App ()
registerExpose h m dd extName = do
  let entrySymbol = Arch.entrySymbol $ P.arch $ Platform.getPlatform $ Global.platformHandle (globalHandle h)
  when (EN.reify extName == entrySymbol) $
    raiseError m $
      "`" <> EN.reify extName <> "` is the entry point of the target and cannot be exposed"
  when (EN.isLLVMIntrinsic extName) $
    raiseError m $
      "`" <> EN.reify extName <> "` is reserved for LLVM intrinsics and cannot be exposed"
  Expose.insert (Global.exposeHandle (globalHandle h)) m dd extName
