module Language.Term.DataInfo
  ( specializeUnary,
    brokenUnaryMessage,
  )
where

import App.App (App)
import App.Run (raiseCritical)
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Language.Common.Binder
import Language.Common.DataInfo qualified as DI
import Language.Common.DefiniteDescription qualified as DD
import Language.Term.Subst qualified as Subst
import Language.Term.Term qualified as TM
import Logger.Hint

specializeUnary ::
  Subst.Handle ->
  Hint ->
  DD.DefiniteDescription ->
  DI.DataInfo (BinderF TM.Type) ->
  [TM.Type] ->
  App TM.Type
specializeUnary h m dataName dataInfo dataArgs = do
  let dataBinders = DI.dataParams dataInfo
  if length dataBinders /= length dataArgs
    then raiseCritical m $ "Arity mismatch while specializing the unary data `" <> DD.reify dataName <> "`"
    else case DI.consInfoList dataInfo of
      [DI.ConsInfo {DI.consParams = [(_, _, _, t)]}] -> do
        let sub = Subst.makeTypeSubst (map getBinderIdent dataBinders) dataArgs
        liftIO $ Subst.substType h sub t
      _ ->
        raiseCritical m $ brokenUnaryMessage dataName

brokenUnaryMessage :: DD.DefiniteDescription -> T.Text
brokenUnaryMessage dataName =
  "Found a broken unary data metadata for `" <> DD.reify dataName <> "`"
