module Move.Context.Decl
  ( initialize,
    insWeakDeclEnv,
  )
where

import Control.Monad
import Data.HashMap.Strict qualified as Map
import Move.Context.App
import Move.Context.App.Internal
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Rule.DeclarationName qualified as DN
import Rule.Foreign qualified as F
import Rule.ForeignCodType qualified as FCT
import Rule.WeakTerm qualified as WT
import Prelude hiding (lookup, read)

initialize :: App ()
initialize = do
  writeRef' weakDeclEnv Map.empty
  arch <- toApp $ Env.getArch Nothing
  forM_ (F.defaultWeakForeignList arch) $ \(F.Foreign _ name domList cod) -> do
    insWeakDeclEnv (DN.Ext name) domList cod

insWeakDeclEnv :: DN.DeclarationName -> [WT.WeakTerm] -> FCT.ForeignCodType WT.WeakTerm -> App ()
insWeakDeclEnv k domList cod =
  modifyRef' weakDeclEnv $ Map.insert k (domList, cod)
