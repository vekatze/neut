module Entity.Command where

import Entity.Config.Add qualified as Add
import Entity.Config.Archive qualified as Archive
import Entity.Config.Build qualified as Build
import Entity.Config.Check qualified as Check
import Entity.Config.Clean qualified as Clean
import Entity.Config.Create qualified as Create
import Entity.Config.LSP qualified as LSP
import Entity.Config.Version qualified as Version

data Command
  = Build Build.Config
  | Check Check.Config
  | Clean Clean.Config
  | Archive Archive.Config
  | Add Add.Config
  | Create Create.Config
  | LSP LSP.Config
  | ShowVersion Version.Config
