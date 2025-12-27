module Kernel.Elaborate.Internal.Handle.Def
  ( Handle,
    new,
    insert',
    get',
    isTemplate',
  )
where

import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Opacity qualified as O
import Language.Term.Inline qualified as Inline
import Language.Term.Term qualified as TM
import Prelude hiding (lookup, read)

newtype Handle = Handle
  { defMapRef :: IORef (Map.HashMap DD.DefiniteDescription Inline.DefInfo)
  }

new :: IO Handle
new = do
  defMapRef <- newIORef Map.empty
  return $ Handle {..}

insert' :: Handle -> O.Opacity -> DD.DefiniteDescription -> [BinderF TM.Term] -> TM.Term -> TM.Term -> Bool -> IO ()
insert' h opacity name xts e typ isTemplateFlag =
  when (opacity == O.Clear) $ do
    let defInfo =
          Inline.DefInfo
            { Inline.defBinders = xts,
              Inline.defBody = e,
              Inline.codType = typ,
              Inline.isTemplate = isTemplateFlag
            }
    modifyIORef' (defMapRef h) $
      Map.insert name defInfo

get' :: Handle -> IO (Map.HashMap DD.DefiniteDescription Inline.DefInfo)
get' h =
  readIORef (defMapRef h)

isTemplate' :: Handle -> DD.DefiniteDescription -> IO Bool
isTemplate' h name = do
  defMap <- readIORef (defMapRef h)
  case Map.lookup name defMap of
    Just defInfo -> return $ Inline.isTemplate defInfo
    Nothing -> return False
