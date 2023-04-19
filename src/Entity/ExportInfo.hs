module Entity.ExportInfo where

import Data.Text qualified as T
import Entity.DefiniteDescription qualified as DD
import Entity.GlobalLocator qualified as GL
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.LocalLocator qualified as LL

type WeakExportInfo =
  (Hint, AliasName, VarOrDD)

type ExportInfo =
  (Hint, AliasName, DD.DefiniteDescription, GN.GlobalName)

type VarOrDD =
  (Either T.Text (GL.GlobalLocator, LL.LocalLocator))

type AliasName =
  DD.DefiniteDescription
