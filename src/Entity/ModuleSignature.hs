module Entity.ModuleSignature where

import Entity.ModuleAlias
import Entity.ModuleDigest

data ModuleSignature
  = Alias ModuleAlias
  | Digest ModuleDigest
