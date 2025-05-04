module Language.RawTerm.Rule.Handle (Handle (..)) where

import Gensym.Rule.Handle qualified as Gensym

newtype Handle
  = InternalHandle
  { _gensymHandle :: Gensym.Handle
  }
