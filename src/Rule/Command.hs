module Rule.Command (Command (..)) where

import Rule.Config.Archive qualified as Archive
import Rule.Config.Build qualified as Build
import Rule.Config.Check qualified as Check
import Rule.Config.Clean qualified as Clean
import Rule.Config.Create qualified as Create
import Rule.Config.Format qualified as Format
import Rule.Config.Get qualified as Get
import Rule.Config.Version qualified as Version
import Rule.Config.Zen qualified as Zen

data Command
  = Build Build.Config
  | Check Check.Config
  | Clean Clean.Config
  | Archive Archive.Config
  | Get Get.Config
  | Create Create.Config
  | Format Format.Config
  | LSP
  | ShowVersion Version.Config
  | Zen Zen.Config
