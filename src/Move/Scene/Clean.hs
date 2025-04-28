module Move.Scene.Clean
  ( Handle,
    new,
    clean,
  )
where

import Control.Monad
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.Locator qualified as Locator
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.Path qualified as Path
import Move.Context.Tag qualified as Tag
import Move.Context.Unused qualified as Unused
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Unravel qualified as Unravel
import Path.IO
import Rule.Module (extractModule)
import Prelude hiding (log)

data Handle = Handle
  { envHandle :: Env.Handle,
    unravelHandle :: Unravel.Handle
  }

new :: Env.Handle -> Gensym.Handle -> Debug.Handle -> Locator.Handle -> OptimizableData.Handle -> KeyArg.Handle -> Unused.Handle -> Tag.Handle -> Antecedent.Handle -> App Handle
new envHandle gensymHandle debugHandle locatorHandle optDataHandle keyArgHandle unusedHandle tagHandle antecedentHandle = do
  unravelHandle <- Unravel.new envHandle gensymHandle debugHandle locatorHandle optDataHandle keyArgHandle unusedHandle tagHandle antecedentHandle
  return $ Handle {..}

clean :: Handle -> EIO ()
clean h = do
  mainModule <- Env.getMainModule (envHandle h)
  moduleList <- Unravel.unravelModule (unravelHandle h) (extractModule mainModule)
  forM_ moduleList $ \someModule -> do
    baseBuildDir <- Path.getBaseBuildDir someModule
    b <- doesDirExist baseBuildDir
    when b $ do
      removeDirRecur baseBuildDir
    ensureDir baseBuildDir
