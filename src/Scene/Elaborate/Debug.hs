module Scene.Elaborate.Debug
  ( viewStmt,
    printConstraints,
  )
where

import Context.App
import Context.Elaborate (getConstraintEnv)
import Context.Remark (printNote')
import Context.Remark qualified as Remark
import Control.Comonad.Cofree
import Control.Monad
import Rule.Attr.Lam qualified as AttrL
import Rule.Constraint
import Rule.DefiniteDescription qualified as DD
import Rule.LamKind qualified as LK
import Rule.Stmt
import Rule.WeakTerm qualified as WT
import Rule.WeakTerm.ToText

viewStmt :: WeakStmt -> App ()
viewStmt stmt = do
  case stmt of
    WeakStmtDefine _ _ m x impArgs expArgs codType e -> do
      let attr = AttrL.Attr {lamKind = LK.Normal codType, identity = 0}
      Remark.printNote m $ DD.reify x <> "\n" <> toText (m :< WT.Pi impArgs expArgs codType) <> "\n" <> toText (m :< WT.PiIntro attr impArgs expArgs e)
    _ ->
      return ()

printConstraints :: App ()
printConstraints = do
  cs <- getConstraintEnv
  forM_ cs $ \c -> do
    case c of
      Actual {} ->
        return ()
      Eq expected actual -> do
        printNote' $ toText expected <> " == " <> toText actual
      Integer {} ->
        return ()
