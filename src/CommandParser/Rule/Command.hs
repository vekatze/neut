module CommandParser.Rule.Command
  ( Command (..),
    InternalCommand (..),
    ExternalCommand (..),
  )
where

import CommandParser.Rule.Config.Archive qualified as Archive
import CommandParser.Rule.Config.Build qualified as Build
import CommandParser.Rule.Config.Check qualified as Check
import CommandParser.Rule.Config.Clean qualified as Clean
import CommandParser.Rule.Config.Create qualified as Create
import CommandParser.Rule.Config.Format qualified as Format
import CommandParser.Rule.Config.Get qualified as Get
import CommandParser.Rule.Config.Remark qualified as Remark
import CommandParser.Rule.Config.Version qualified as Version
import CommandParser.Rule.Config.Zen qualified as Zen

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
