module Main (main) where

import Act.Archive qualified as Archive
import Act.Build qualified as Build
import Act.Check qualified as Check
import Act.Clean qualified as Clean
import Act.Create qualified as Create
import Act.Format qualified as Format
import Act.Get qualified as Get
import Act.LSP qualified as LSP
import Act.Version qualified as Version
import Act.Zen qualified as Zen
import Context.App
import Context.External qualified as External
import Context.OptParse qualified as OptParse
import Context.Throw qualified as Throw
import Rule.Command qualified as C
import System.IO

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  Main.execute

execute :: IO ()
execute = do
  runApp $ do
    c <- OptParse.parseCommand
    Throw.run $ do
      External.ensureExecutables
      case c of
        C.Build cfg -> do
          Build.build cfg
        C.Check cfg -> do
          Check.check cfg
        C.Clean cfg ->
          Clean.clean cfg
        C.Archive cfg ->
          Archive.archive cfg
        C.Create cfg ->
          Create.create cfg
        C.Get cfg ->
          Get.get cfg
        C.Format cfg ->
          Format.format cfg
        C.LSP ->
          LSP.lsp
        C.ShowVersion cfg ->
          Version.showVersion cfg
        C.Zen cfg ->
          Zen.zen cfg
