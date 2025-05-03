module Color.Rule.Handle
  ( Handle (..),
  )
where

data Handle
  = InternalHandle
  { _shouldColorizeStdout :: Bool,
    _shouldColorizeStderr :: Bool
  }
