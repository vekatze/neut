module Move.Scene.Init.Target
  ( Handle,
    new,
    initializeForTarget,
  )
where

import Control.Monad.IO.Class
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.Definition qualified as Definition
import Move.Context.Env qualified as Env
import Move.Context.Locator qualified as Locator
import Move.Context.Type qualified as Type
import Move.Context.WeakDefinition qualified as WeakDefinition
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Clarify qualified as Clarify
import Move.Scene.Unravel qualified as Unravel
import Move.UI.Handle.GlobalRemark qualified as GlobalRemark

data Handle
  = Handle
  { clarifyHandle :: Clarify.Handle,
    unravelHandle :: Unravel.Handle,
    antecedentHandle :: Antecedent.Handle,
    globalRemarkHandle :: GlobalRemark.Handle,
    weakDefinitionHandle :: WeakDefinition.Handle,
    definitionHandle :: Definition.Handle,
    typeHandle :: Type.Handle
  }

new :: Env.Handle -> Gensym.Handle -> Locator.Handle -> App Handle
new envHandle gensymHandle locatorHandle = do
  clarifyHandle <- Clarify.new envHandle gensymHandle locatorHandle
  unravelHandle <- Unravel.new envHandle gensymHandle
  antecedentHandle <- Antecedent.new
  globalRemarkHandle <- GlobalRemark.new
  weakDefinitionHandle <- WeakDefinition.new gensymHandle
  definitionHandle <- Definition.new
  typeHandle <- Type.new
  return $ Handle {..}

initializeForTarget :: Handle -> IO ()
initializeForTarget h = do
  liftIO $ Clarify.registerFoundationalTypes (clarifyHandle h)
  Unravel.initialize (unravelHandle h)
  Antecedent.initialize (antecedentHandle h)
  liftIO $ GlobalRemark.set (globalRemarkHandle h) []
  WeakDefinition.initialize (weakDefinitionHandle h)
  Definition.initialize (definitionHandle h)
  Type.initialize (typeHandle h)
