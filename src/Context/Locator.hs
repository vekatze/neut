module Context.Locator where

import qualified Context.Throw as Throw
import Control.Monad.IO.Class
import qualified Entity.BaseName as BN
import qualified Entity.DefiniteDescription as DD
import Entity.DefiniteLocator as DL
import qualified Entity.LocalLocator as LL
import Entity.Module
import qualified Entity.Section as S
import Entity.Source
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

data Context = Context
  { withSection :: forall a m. MonadIO m => S.Section -> m a -> m a,
    attachCurrentLocator :: BN.BaseName -> IO DD.DefiniteDescription,
    activateGlobalLocator :: SGL.StrictGlobalLocator -> IO (),
    activateDefiniteLocator :: DL.DefiniteLocator -> IO (),
    clearActiveLocators :: IO (),
    getPossibleReferents :: LL.LocalLocator -> IO [DD.DefiniteDescription]
  }

data Config = Config
  { mainModule :: Module,
    currentSource :: Source,
    throwCtx :: Throw.Context
  }
