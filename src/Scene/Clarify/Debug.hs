module Scene.Clarify.Debug (printCompDef) where

import Context.App
import Context.Remark (printNote')
import Data.Text qualified as T
import Entity.Comp qualified as C
import Entity.DefiniteDescription qualified as DD

printCompDef :: C.CompDef -> App ()
printCompDef (x, (_, args, e)) = do
  printNote' "==================="
  printNote' $ DD.reify x
  printNote' $ T.pack $ show args
  printNote' $ T.pack $ show e
