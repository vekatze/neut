module Main.Rule.Command
  ( Command (..),
    InternalCommand (..),
    ExternalCommand (..),
  )
where

import Main.Rule.Config.Archive qualified as Archive
import Main.Rule.Config.Build qualified as Build
import Main.Rule.Config.Check qualified as Check
import Main.Rule.Config.Clean qualified as Clean
import Main.Rule.Config.Create qualified as Create
import Main.Rule.Config.Format qualified as Format
import Main.Rule.Config.Get qualified as Get
import Main.Rule.Config.Remark qualified as Remark
import Main.Rule.Config.Version qualified as Version
import Main.Rule.Config.Zen qualified as Zen

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
