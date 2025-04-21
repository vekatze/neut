module Scene.Clarify.Debug (printCompDef) where

import Context.App
import Context.Remark (printNote')
import Data.Text qualified as T
import Rule.Comp qualified as C
import Rule.DefiniteDescription qualified as DD

printCompDef :: C.CompStmt -> App ()
printCompDef stmt = do
  case stmt of
    C.Def x _ args e -> do
      printNote' "==================="
      printNote' $ DD.reify x
      printNote' $ T.pack $ show args
      printNote' $ T.pack $ show e
    C.Foreign {} -> do
      printNote' "<foreign>"
