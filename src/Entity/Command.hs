module Entity.Command (Command (..)) where

import Entity.Config.Archive qualified as Archive
import Entity.Config.Build qualified as Build
import Entity.Config.Check qualified as Check
import Entity.Config.Clean qualified as Clean
import Entity.Config.Create qualified as Create
import Entity.Config.Format qualified as Format
import Entity.Config.Get qualified as Get
import Entity.Config.Version qualified as Version
import Entity.Config.Zen qualified as Zen

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
