module Rule.Command
  ( Command (..),
    InternalCommand (..),
    ExternalCommand (..),
  )
where

import Rule.Config.Archive qualified as Archive
import Rule.Config.Build qualified as Build
import Rule.Config.Check qualified as Check
import Rule.Config.Clean qualified as Clean
import Rule.Config.Create qualified as Create
import Rule.Config.Format qualified as Format
import Rule.Config.Get qualified as Get
import Rule.Config.Remark qualified as Remark
import Rule.Config.Version qualified as Version
import Rule.Config.Zen qualified as Zen

data InternalCommand
  = Archive Archive.Config
  | Build Build.Config
  | Check Check.Config
  | Clean Clean.Config
  | Format Format.Config
  | Get Get.Config
  | LSP
  | Zen Zen.Config

data ExternalCommand
  = Create Create.Config
  | ShowVersion Version.Config

data Command
  = Internal Remark.Config InternalCommand
  | External Remark.Config ExternalCommand
