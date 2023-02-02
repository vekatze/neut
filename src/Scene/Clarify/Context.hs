module Scene.Clarify.Context
  ( Context (..),
  )
where

import qualified Context.CompDefinition as CompDefinition
import qualified Context.Gensym as Gensym
import qualified Context.Locator as Locator
import qualified Entity.DefiniteDescription as DD

class
  ( CompDefinition.Context m,
    Gensym.Context m,
    Locator.Context m
  ) =>
  Context m
  where
  initialize :: m ()
  getAuxEnv :: m CompDefinition.DefMap
  insertToAuxEnv :: CompDefinition.DefKey -> CompDefinition.DefValue -> m ()
  isAlreadyRegistered :: DD.DefiniteDescription -> m Bool
