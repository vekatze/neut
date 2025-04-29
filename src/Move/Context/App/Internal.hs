module Move.Context.App.Internal
  ( Env,
    newEnv,
  )
where

data Env = Env {}

newEnv :: IO Env
newEnv = do
  return Env {}
