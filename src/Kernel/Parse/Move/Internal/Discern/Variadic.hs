module Kernel.Parse.Move.Internal.Discern.Variadic (defineVariadic) where

import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Opacity qualified as O
import Language.Common.Rule.RuleKind (RuleKind)
import Language.Common.Rule.StmtKind qualified as SK
import Language.RawTerm.Rule.RawStmt
import Language.RawTerm.Rule.RawTerm qualified as RT
import Logger.Rule.Hint
import SyntaxTree.Rule.Series qualified as SE

defineVariadic ::
  RuleKind ->
  Hint ->
  DD.DefiniteDescription ->
  (RT.RawTerm, RT.RawTerm) ->
  (RT.RawTerm, RT.RawTerm) ->
  Loc ->
  [PostRawStmt]
defineVariadic kind m name (node, nodeType) (tip, tipType) loc = do
  let nodeDef = makeDef m (DD.getNodeDD name) node nodeType loc
  let tipDef = makeDef m (DD.getTipDD name) tip tipType loc
  [PostRawStmtVariadic kind m name, nodeDef, tipDef]

makeDef :: Hint -> DD.DefiniteDescription -> RT.RawTerm -> RT.RawTerm -> Loc -> PostRawStmt
makeDef m name e t loc = do
  let m' = blur m
  let rawDef =
        RT.RawDef
          { geist =
              RT.RawGeist
                { loc = m',
                  name = (name, []),
                  isConstLike = False,
                  impArgs = (SE.emptySeriesAC, []),
                  expArgs = (SE.emptySeriesPC, []),
                  cod = ([], t)
                },
            leadingComment = [],
            body = e,
            trailingComment = [],
            endLoc = loc
          }
  PostRawStmtDefine [] (SK.Normal O.Clear) rawDef
