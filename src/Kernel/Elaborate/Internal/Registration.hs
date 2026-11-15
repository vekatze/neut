{-# LANGUAGE DerivingVia #-}

module Kernel.Elaborate.Internal.Registration
  ( Registration (..),
    register,
  )
where

import App.App (App)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Generics (Generic, Generically (..))
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Data qualified as Data
import Kernel.Common.Handle.Global.Resource qualified as Resource
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Elaborate.Internal.Handle.Def qualified as Definition
import Kernel.Elaborate.Internal.Handle.Elaborate
import Kernel.Elaborate.Internal.Handle.Trope qualified as Trope
import Kernel.Elaborate.Internal.Handle.TypeDef qualified as TypeDef
import Kernel.Elaborate.Internal.Handle.WeakDecl qualified as WeakDecl
import Kernel.Elaborate.Internal.Handle.WeakTypeDef qualified as WeakTypeDef
import Language.Common.Binder
import Language.Common.DataInfo qualified as DI
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Foreign qualified as F
import Language.LowComp.DeclarationName qualified as DN
import Language.Term.Inline.Handle qualified as InlineHandle
import Language.Term.Stmt qualified as Stmt
import Language.Term.Term qualified as TM
import Language.WeakTerm.WeakTerm qualified as WT

data Registration = Registration
  { typeEntries :: [(DD.DefiniteDescription, WT.WeakType)],
    definitionEntries :: [(DD.DefiniteDescription, InlineHandle.DefInfo)],
    typeDefEntries :: [(DD.DefiniteDescription, TypeDef.TypeDefInfo)],
    dataInfoEntries :: [(DD.DefiniteDescription, DI.DataInfo (BinderF TM.Type))],
    resourceEntries :: [(DD.DefiniteDescription, Resource.ResourceLayout)],
    tropeEntries :: [(DD.DefiniteDescription, [Stmt.DefineMeta])],
    weakTypeDefEntries :: [(DD.DefiniteDescription, WeakTypeDef.TypeDef)],
    weakDataInfoEntries :: [(DD.DefiniteDescription, DI.DataInfo (BinderF WT.WeakType))],
    foreignEntries :: [(DN.DeclarationName, F.ForeignSignature WT.WeakType)]
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via (Generically Registration)

register :: Handle -> Registration -> App ()
register h registration = do
  liftIO $ Type.insertMany (typeHandle h) $ typeEntries registration
  liftIO $ Definition.insertMany (defHandle h) $ definitionEntries registration
  liftIO $ Data.insertMany (dataHandle h) $ dataInfoEntries registration
  liftIO $ Resource.insertMany (Global.resourceHandle $ globalHandle h) $ resourceEntries registration
  liftIO $ TypeDef.insertMany (typeDefHandle h) $ typeDefEntries registration
  liftIO $ Trope.insertMany (tropeHandle h) $ tropeEntries registration
  liftIO $ Data.insertWeakMany (dataHandle h) $ weakDataInfoEntries registration
  liftIO $ WeakTypeDef.insertMany (weakTypeDefHandle h) $ weakTypeDefEntries registration
  liftIO $ forM_ (foreignEntries registration) $ uncurry (WeakDecl.insert (weakDeclHandle h))
