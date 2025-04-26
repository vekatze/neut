module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
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
import Move.Console.EnsureExecutables (ensureExecutables)
import Move.Context.App
import Move.Context.EIO (toApp)
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
    c <- liftIO OptParse.parseCommand
    Throw.run $ do
      ensureExecutables
      case c of
        C.Build cfg -> do
          Build.build cfg
        C.Check cfg -> do
          Check.check cfg
        C.Clean cfg -> do
          h <- Clean.new
          toApp $ Clean.clean h cfg
        C.Archive cfg -> do
          h <- Archive.new
          toApp $ Archive.archive h cfg
        C.Create cfg ->
          Create.create cfg
        C.Get cfg ->
          Get.get cfg
        C.Format cfg ->
          Format.format cfg
        C.LSP ->
          LSP.lsp
        C.ShowVersion cfg ->
          liftIO $ Version.showVersion cfg
        C.Zen cfg ->
          Zen.zen cfg
