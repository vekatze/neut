module Move.Context.Gensym
  ( newCount,
    newIdentFromText,
    newIdentFromIdent,
  )
where

import Control.Monad.Reader
import Data.IORef
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal
import Rule.Ident
import Rule.Ident.Reify qualified as Ident

newCount :: App Int
newCount =
  asks counter >>= \ref -> liftIO $ atomicModifyIORef' ref (\x -> (x + 1, x))

{-# INLINE newIdentFromText #-}
newIdentFromText :: T.Text -> App Ident
newIdentFromText s = do
  i <- newCount
  return $ I (s, i)

{-# INLINE newIdentFromIdent #-}
newIdentFromIdent :: Ident -> App Ident
newIdentFromIdent x =
  newIdentFromText (Ident.toText x)
