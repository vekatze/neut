module Move.Scene.Init.Target
  ( Handle,
    new,
    initializeForTarget,
  )
where

import Control.Monad.IO.Class
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.Definition qualified as Definition
import Move.Context.Type qualified as Type
import Move.Context.WeakDefinition qualified as WeakDefinition
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

new ::
  Clarify.Handle ->
  Unravel.Handle ->
  Antecedent.Handle ->
  GlobalRemark.Handle ->
  WeakDefinition.Handle ->
  Definition.Handle ->
  Type.Handle ->
  Handle
new clarifyHandle unravelHandle antecedentHandle globalRemarkHandle weakDefinitionHandle definitionHandle typeHandle = do
  Handle {..}

initializeForTarget :: Handle -> IO ()
initializeForTarget h = do
  liftIO $ Clarify.registerFoundationalTypes (clarifyHandle h)
  Unravel.initialize (unravelHandle h)
  Antecedent.initialize (antecedentHandle h)
  liftIO $ GlobalRemark.set (globalRemarkHandle h) []
  WeakDefinition.initialize (weakDefinitionHandle h)
  Definition.initialize (definitionHandle h)
  Type.initialize (typeHandle h)
