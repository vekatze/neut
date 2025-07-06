module Color.Handle
  ( Handle (..),
  )
where

data Handle = InternalHandle
  { _shouldColorizeStdout :: Bool,
    _shouldColorizeStderr :: Bool
  }
