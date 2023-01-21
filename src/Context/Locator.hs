module Context.Locator where

import Control.Monad.Identity
import Control.Monad.Trans
import Entity.BaseName qualified as BN
import Entity.DefiniteDescription qualified as DD
import Entity.DefiniteLocator as DL
import Entity.LocalLocator qualified as LL
import Entity.Section qualified as S
import Entity.Source qualified as Source
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
