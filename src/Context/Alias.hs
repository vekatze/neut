module Context.Alias where

import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import qualified Data.Text as T
import Entity.Hint
import Entity.Module

data Config = Config
  { currentModule :: Module,
    mainModule :: Module,
    throwCtx :: Throw.Context,
    locatorCtx :: Locator.Context
  }

data Context = Context
  { getCandList :: T.Text -> Bool -> IO [T.Text], -- get all possible referants, with aliases resolved
    registerLocatorAlias :: Hint -> T.Text -> T.Text -> IO ()
  }
