module Aux.CommandParser.Rule.Command
  ( Command (..),
    InternalCommand (..),
    ExternalCommand (..),
  )
where

import Aux.CommandParser.Rule.Config.Archive qualified as Archive
import Aux.CommandParser.Rule.Config.Build qualified as Build
import Aux.CommandParser.Rule.Config.Check qualified as Check
import Aux.CommandParser.Rule.Config.Clean qualified as Clean
import Aux.CommandParser.Rule.Config.Create qualified as Create
import Aux.CommandParser.Rule.Config.FormatEns qualified as FormatEns
import Aux.CommandParser.Rule.Config.FormatSource qualified as FormatSource
import Aux.CommandParser.Rule.Config.Get qualified as Get
import Aux.CommandParser.Rule.Config.Remark qualified as Remark
import Aux.CommandParser.Rule.Config.Version qualified as Version
import Aux.CommandParser.Rule.Config.Zen qualified as Zen

data InternalCommand
  = Archive Archive.Config
  | Build Build.Config
  | Check Check.Config
  | Clean Clean.Config
  | FormatSource FormatSource.Config
  | FormatEns FormatEns.Config
  | Get Get.Config
  | LSP
  | Zen Zen.Config

data ExternalCommand
  = Create Create.Config
  | ShowVersion Version.Config

data Command
  = Internal Remark.Config InternalCommand
  | External Remark.Config ExternalCommand
