module Entity.ModuleSignature where

import Entity.ModuleAlias
import Entity.ModuleChecksum

data ModuleSignature
  = Alias ModuleAlias
  | Checksum ModuleChecksum
