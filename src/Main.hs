module Main (main) where

import Move.Act.Archive qualified as Archive
import Move.Act.Build qualified as Build
import Move.Act.Check qualified as Check
import Move.Act.Clean qualified as Clean
import Move.Act.Create qualified as Create
import Move.Act.Format qualified as Format
import Move.Act.Get qualified as Get
import Move.Act.LSP qualified as LSP
import Move.Act.Version qualified as Version
import Move.Act.Zen qualified as Zen
import Move.Context.App
import Move.Context.External qualified as External
import Move.Context.OptParse qualified as OptParse
import Move.Context.Throw qualified as Throw
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
