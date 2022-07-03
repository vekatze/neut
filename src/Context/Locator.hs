module Context.Locator where

import qualified Context.Throw as Throw
import qualified Data.Text as T
import Entity.Hint

-- the structure of a name of a global variable:
--
--     some.path.to.item::foo.bar.buz.qux.some-function
--                            ---
--                            ↑ section name
--     -----------------  ---------------
--     ↑ global locator   ↑ local locator
--     ----------------------
--     ↑ partial locator
--     --------------------------
--     ↑ partial locator
--     ------------------------------
--     ↑ partial locator
--     ----------------------------------
--     ↑ locator
--     ------------------------------------------------
--     ↑ the definite description of a global variable `some-function` (up-to module alias)

-- terms:
--   - current locator
--     - current global locator: the global locator of the file that is being parsed
--     - current local locator: the local locator of the current section that is being parsed
--   - active locator
--     - active global locator: a global locator that is used when resolving global names
--     - active local locator: a local locator that is used when resolving global names

type VarName = T.Text

type IsDefinite = Bool

data Axis = Axis
  { pushToCurrentLocalLocator :: T.Text -> IO (),
    popFromCurrentLocalLocator :: Hint -> IO T.Text,
    setCurrentGlobalLocator :: T.Text -> IO (),
    attachCurrentLocator :: VarName -> IO VarName,
    activateGlobalLocator :: T.Text -> IO (),
    activatePartialLocator :: T.Text -> IO (),
    clearActiveLocators :: IO (),
    getPossibleReferents :: VarName -> IsDefinite -> IO [VarName]
  }

data Handler = Handler
  { open :: Config -> IO Axis,
    close :: Axis -> IO ()
  }

data Config = Config
  { currentGlobalLocator :: T.Text,
    currentLocalLocator :: [T.Text],
    throwCtx :: Throw.Context
  }
