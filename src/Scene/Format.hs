module Scene.Format (format) where

import Context.App
import Context.Parse (readSourceFile)
import Control.Monad
import Data.Text qualified as T
import Entity.RawProgram.Decode qualified as RawProgram
import Path.IO
import Scene.Parse.Core qualified as P
import Scene.Parse.Program qualified as Parse
import Prelude hiding (log)

format :: FilePath -> App T.Text
format path = do
  path' <- resolveFile' path
  content <- readSourceFile path'
  program <- P.parseFile True Parse.parseProgram path' content
  return $ RawProgram.pp program
