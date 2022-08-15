module Context.Locator where

import Control.Monad.Identity
import Control.Monad.Trans
import qualified Entity.BaseName as BN
import qualified Entity.DefiniteDescription as DD
import Entity.DefiniteLocator as DL
import qualified Entity.LocalLocator as LL
import qualified Entity.Section as S
import qualified Entity.Source as Source
import Entity.StrictGlobalLocator as SGL

-- the structure of a name of a global variable:
--
--     some.path.to.item::foo.bar.buz.qux.some-function
--                            ---
--                            ↑ section name
--     -----------------  ---------------
--     ↑ global locator   ↑ local locator
--     ----------------------------------
--     ↑ definite locator
--     ------------------------------------------------
--     ↑ the definite description of a global variable `some-function` (up-to module alias)

-- terms:
--   - current locator
--     - current global locator: the global locator of the file that is being parsed
--     - current local locator: the local locator of the current section that is being parsed
--   - active locator
--     - active global locator: a global locator that is used when resolving global names
--     - active local locator: a local locator that is used when resolving global names

class Monad m => Context m where
  initialize :: m ()
  withLiftedSection :: (MonadTrans t, Monad (t m)) => S.Section -> t m a -> t m a
  attachCurrentLocator :: BN.BaseName -> m DD.DefiniteDescription
  activateGlobalLocator :: SGL.StrictGlobalLocator -> m ()
  activateDefiniteLocator :: DL.DefiniteLocator -> m ()
  clearActiveLocators :: m ()
  getPossibleReferents :: LL.LocalLocator -> m [DD.DefiniteDescription]
  getMainDefiniteDescription :: Source.Source -> m (Maybe DD.DefiniteDescription)
  withSection :: S.Section -> m a -> m a
  withSection section action =
    runIdentityT $ withLiftedSection section $ lift action

-- withSection :: forall a m. MonadIO m => S.Section -> m a -> m a
-- data Config = Config
--   { mainModule :: Module,
--     currentSource :: Source,
--     throwCtx :: Throw.Context,
--     pathCtx :: Path.Context,
--     moduleCtx :: Module.Context
--   }

-- getMainDefiniteDescription :: Context -> IO DD.DefiniteDescription
-- getMainDefiniteDescription ctx = do
--   attachCurrentLocator ctx BN.main

-- isMainFile :: Context -> Source -> IO Bool
-- isMainFile ctx source = do
--   sourcePathList <- mapM (getSourcePath ctx) $ Map.elems $ moduleTarget (sourceModule source)
--   return $ elem (sourceFilePath source) sourcePathList
