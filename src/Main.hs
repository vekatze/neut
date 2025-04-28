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
import Move.Context.Env qualified as Env
import Move.Context.OptParse qualified as OptParse
import Move.Context.Throw qualified as Throw
import Move.Language.Utility.Gensym qualified as Gensym
import Rule.Command qualified as C
import System.IO

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  Main.execute

execute :: IO ()
execute = do
  runApp $ do
    gensymHandle <- liftIO Gensym.new
    envHandle <- Env.new
    c <- liftIO OptParse.parseCommand
    Throw.run $ do
      ensureExecutables
      case c of
        C.Build cfg -> do
          h <- Build.new cfg envHandle gensymHandle
          Build.build h cfg
        C.Check cfg -> do
          h <- Check.new envHandle gensymHandle
          Check.check h cfg
        C.Clean cfg -> do
          h <- Clean.new gensymHandle
          toApp $ Clean.clean h cfg
        C.Archive cfg -> do
          h <- Archive.new envHandle gensymHandle
          toApp $ Archive.archive h cfg
        C.Create cfg -> do
          h <- Create.new gensymHandle
          Create.create h cfg
        C.Get cfg -> do
          h <- Get.new envHandle gensymHandle
          Get.get h cfg
        C.Format cfg -> do
          h <- Format.new gensymHandle
          toApp $ Format.format h cfg
        C.LSP -> do
          h <- LSP.new envHandle gensymHandle
          LSP.lsp h
        C.ShowVersion cfg ->
          liftIO $ Version.showVersion cfg
        C.Zen cfg -> do
          h <- Zen.new cfg gensymHandle
          Zen.zen h cfg
