{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import Options.Applicative
import Path
import Path.IO
import System.Console.ANSI
import System.Exit
import System.Process
import Text.Read (readMaybe)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.IO as TIO

import Clarify
import Data.Env
import Elaborate
import Emit
import LLVM
import Parse

type ImportOptScreenName = String

type BuildOptInputPath = String

type BuildOptOutputPath = String

data OutputKind
  = OutputKindObject
  | OutputKindLLVM
  deriving (Show)

instance Read OutputKind where
  readsPrec _ "object" = [(OutputKindObject, [])]
  readsPrec _ "llvm" = [(OutputKindLLVM, [])]
  readsPrec _ _ = []

data Command =
  Build BuildOptInputPath (Maybe BuildOptOutputPath) OutputKind

parseBuildOpt :: Parser Command
parseBuildOpt = do
  let inputPathOpt =
        argument str $ mconcat [metavar "INPUT", help "The path of input file"]
  let outputPathOpt =
        optional $
        strOption $
        mconcat
          [ long "output"
          , short 'o'
          , metavar "OUTPUT"
          , help "The path of output file"
          ]
  let outputKindOpt =
        option kindReader $
        mconcat
          [ long "emit"
          , metavar "KIND"
          , value OutputKindObject
          , help "The type of output file"
          ]
  Build <$> inputPathOpt <*> outputPathOpt <*> outputKindOpt

kindReader :: ReadM OutputKind
kindReader = do
  s <- str
  case readMaybe s of
    Nothing -> readerError $ "unknown mode:" ++ s
    Just m -> return m

parseOpt :: Parser Command
parseOpt =
  subparser $
  command
    "build"
    (info
       (helper <*> parseBuildOpt)
       (progDesc "Usage: neut build FILENAME [OPTIONS]"))

optParser :: ParserInfo Command
optParser = info (helper <*> parseOpt) fullDesc

main :: IO ()
main = execParser optParser >>= run

run :: Command -> IO ()
run (Build inputPathStr mOutputPathStr outputKind) = do
  inputPath <- resolveFile' inputPathStr
  resultOrErr <- evalWithEnv (process inputPath) (initialEnv inputPath)
  basename <- setFileExtension "" $ filename inputPath
  mOutputPath <- mapM resolveFile' mOutputPathStr
  outputPath <- constructOutputPath basename mOutputPath outputKind
  case resultOrErr of
    Left err -> do
      seqIO err
      -- printError err
      exitWith (ExitFailure 1)
    Right result -> writeResult result outputPath outputKind

printError :: String -> IO ()
printError err = do
  liftIO $
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
  liftIO $ putStr "error: "
  liftIO $ setSGR [Reset]
  liftIO $ putStrLn $ err

constructOutputPath ::
     Path Rel File -> Maybe (Path Abs File) -> OutputKind -> IO (Path Abs File)
constructOutputPath basename Nothing OutputKindLLVM = do
  dir <- getCurrentDir
  (dir </> basename) <.> "ll"
constructOutputPath basename Nothing OutputKindObject = do
  dir <- getCurrentDir
  return $ dir </> basename
constructOutputPath _ (Just path) _ = return path

writeResult :: [B.ByteString] -> Path Abs File -> OutputKind -> IO ()
writeResult result outputPath OutputKindLLVM = do
  let content = BC.unlines result
  B.writeFile (toFilePath outputPath) content
writeResult result outputPath OutputKindObject = do
  let content = BC.unlines result
  tmpOutputPath <- liftIO $ outputPath <.> "ll"
  let tmpOutputPathStr = toFilePath tmpOutputPath
  B.writeFile tmpOutputPathStr content
  callProcess
    "clang"
    [tmpOutputPathStr, "-Wno-override-module", "-o" ++ toFilePath outputPath]
  removeFile tmpOutputPath

-- process :: Path Abs File -> WithEnv [T.Text]
process :: Path Abs File -> WithEnv [B.ByteString]
process inputPath = do
  content <- liftIO $ TIO.readFile $ toFilePath inputPath
  e <- parse content (toFilePath inputPath) >>= elaborate
  -- p "elaborated"
  e' <- clarify e
  -- p "clarified"
  e'' <- toLLVM e'
  -- p "llvm-done"
  emit e'' -- process input = do
   -- parse >=> elaborate >=> polarize >=> toLLVM >=> emit

seqIO :: [IO ()] -> IO ()
seqIO [] = return ()
seqIO (a:as) = a >> seqIO as
