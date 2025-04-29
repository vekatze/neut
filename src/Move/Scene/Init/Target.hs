module Move.Scene.Init.Target
  ( Handle,
    new,
    initializeForTarget,
  )
where

import Control.Monad.IO.Class
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.Debug qualified as Debug
import Move.Context.Definition qualified as Definition
import Move.Context.Env qualified as Env
import Move.Context.Global qualified as Global
import Move.Context.Locator qualified as Locator
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.Tag qualified as Tag
import Move.Context.Type qualified as Type
import Move.Context.Unused qualified as Unused
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

new :: Env.Handle -> Gensym.Handle -> Debug.Handle -> Locator.Handle -> Global.Handle -> OptimizableData.Handle -> Unused.Handle -> Tag.Handle -> Antecedent.Handle -> Type.Handle -> App Handle
new envHandle gensymHandle debugHandle locatorHandle globalHandle optDataHandle unusedHandle tagHandle antecedentHandle typeHandle = do
  clarifyHandle <- Clarify.new gensymHandle locatorHandle optDataHandle
  unravelHandle <- Unravel.new envHandle gensymHandle debugHandle locatorHandle globalHandle unusedHandle tagHandle antecedentHandle
  globalRemarkHandle <- GlobalRemark.new
  weakDefinitionHandle <- WeakDefinition.new gensymHandle
  definitionHandle <- Definition.new
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
