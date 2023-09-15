module Scene.Parse.RawTerm
  ( reflRawTerm,
    newAxis,
    reflArgList,
    Axis,
  )
where

import Context.App
import Context.Decl (lookupDeclEnv')
import Context.Decl qualified as Decl
import Context.Env qualified as Env
import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.Annotation qualified as Annot
import Entity.Arch qualified as Arch
import Entity.Atom qualified as AT
import Entity.BuildMode qualified as BM
import Entity.Const
import Entity.DataSize qualified as DS
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.Error
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.Hint.Reify qualified as Hint
import Entity.Locator qualified as L
import Entity.LowType qualified as LT
import Entity.Magic qualified as M
import Entity.Name
import Entity.Name qualified as Name
import Entity.OS qualified as OS
import Entity.Opacity qualified as O
import Entity.Platform qualified as Platform
import Entity.RawBinder
import Entity.RawIdent
import Entity.RawLamKind qualified as LK
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.Remark
import Entity.Tree
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Scene.Parse.LowType
import Text.Read (readMaybe)

data Axis = Axis
  { dataSize :: DS.DataSize,
    declEnv :: DN.DeclEnv,
    platform :: Platform.Platform,
    buildMode :: BM.BuildMode
  }

newAxis :: App Axis
newAxis = do
  ds <- Env.getDataSize'
  declEnv <- Decl.getDeclEnv
  bm <- Env.getBuildMode
  return $
    Axis
      { dataSize = ds,
        declEnv = declEnv,
        platform = Platform.platform,
        buildMode = bm
      }

reflRawTerm :: Axis -> Tree -> EE RT.RawTerm
reflRawTerm ax t =
  case t of
    m :< Atom at ->
      reflAtom m at
    m :< Node ts ->
      reflNode ax m ts
    _ :< List _ -> do
      (m, es) <- toList t
      reflListIntro ax m es

reflAtom :: Hint -> AT.Atom -> EE RT.RawTerm
reflAtom m at =
  case at of
    AT.String str -> do
      textType <- locatorToVarGlobal' m coreText
      return $ m :< RT.Prim (WP.Value (WPV.StaticText textType str))
    AT.Symbol sym
      | sym == "tau" ->
          return $ m :< RT.Tau
      | sym == "_" ->
          return $ m :< RT.Hole
      | sym == "admit" ->
          reflAdmit m
      | Just intValue <- readMaybe (T.unpack sym) ->
          return $ m :< RT.Prim (WP.Value (WPV.Int (m :< RT.Hole) intValue))
      | Just floatValue <- readMaybe (T.unpack sym) ->
          return $ m :< RT.Prim (WP.Value (WPV.Float (m :< RT.Hole) floatValue))
      | otherwise ->
          return $ m :< RT.Var (Name.fromText m sym)

reflNode :: Axis -> Hint -> [Tree] -> EE RT.RawTerm
reflNode ax m ts =
  case ts of
    [] ->
      Left $ newError m "empty node"
    headTree : rest -> do
      case headTree of
        _ :< Atom (AT.Symbol sym)
          | sym == "Fn" -> do
              (dom, cod) <- reflArrowArgs' m rest
              dom' <- reflArgList ax dom
              cod' <- reflRawTerm ax cod
              return $ m :< RT.Pi dom' cod'
          | sym == "#fn" -> do
              (dom, body) <- reflArrowArgs' m rest
              dom' <- reflArgList ax dom
              body' <- reflRawTerm ax body
              return $ lam m dom' body'
          | sym == "#mu" -> do
              case rest of
                [] ->
                  Left $ newError m "empty arguments"
                funcName : rest' -> do
                  (mSelf, self') <- getSymbol funcName
                  (dom, body) <- reflArrowArgs' m rest'
                  dom' <- reflArgList ax dom
                  let cod = m :< RT.Hole
                  body' <- reflRawTerm ax body
                  return $ m :< RT.PiIntro (LK.Fix (mSelf, self', cod)) dom' body'
          | sym == "#match" ->
              reflMatch ax m ts
          | sym == "#match&" ->
              reflMatch ax m ts
          | sym == "noema",
            [arg] <- rest -> do
              arg' <- reflRawTerm ax arg
              return $ m :< RT.Noema arg'
          | sym == "embody",
            [arg] <- rest -> do
              arg' <- reflRawTerm ax arg
              return $ m :< RT.Embody arg'
          | sym == "#let" -> do
              let (rest', attrs) = splitAttrs rest
              case rest' of
                [arg, body, cont] -> do
                  (mArg, arg') <- getSymbol arg
                  body' <- reflRawTerm ax body
                  cont' <- reflRawTerm ax cont
                  let t = mArg :< RT.Hole
                  nxs <- getNoeticArgs attrs
                  return $ m :< RT.Let (mArg, arg', t) nxs body' cont'
                _ ->
                  Left $ newError m "#let"
          | sym == "magic",
            (headSym : args) <- rest ->
              reflMagic ax m headSym args
          | sym == "#introspect",
            key : clauses <- rest ->
              reflIntrospect ax m key clauses
        _ -> do
          func <- reflRawTerm ax headTree
          args <- mapM (reflRawTerm ax) rest
          return $ m :< RT.PiElim func args

getNoeticArgs :: Map.HashMap T.Text Tree -> EE [(Hint, T.Text)]
getNoeticArgs attrs =
  case Map.lookup "on" attrs of
    Nothing ->
      return []
    Just noeticArgs -> do
      (m, noeticArgs') <- toList noeticArgs
      noeticArgs'' <- mapM getSymbol noeticArgs'
      ensureIdentLinearity m S.empty (map snd noeticArgs'')
      return noeticArgs''

ensureIdentLinearity :: Hint -> S.Set RawIdent -> [RawIdent] -> EE ()
ensureIdentLinearity m foundVarSet vs =
  case vs of
    [] ->
      return ()
    name : rest
      | S.member name foundVarSet ->
          Left $ newError m $ "found a non-linear occurrence of `" <> name <> "`."
      | otherwise ->
          ensureIdentLinearity m (S.insert name foundVarSet) rest

reflMagic :: Axis -> Hint -> Tree -> [Tree] -> EE RT.RawTerm
reflMagic ax m t rest = do
  (_, sym) <- getSymbol t
  case sym of
    "cast" ->
      case rest of
        [from, to, term] -> do
          from' <- reflRawTerm ax from
          to' <- reflRawTerm ax to
          term' <- reflRawTerm ax term
          return $ m :< RT.Magic (M.Cast from' to' term')
        _ ->
          Left $ newError m "arity mismatch"
    "store" ->
      case rest of
        [lt, value, ptr] -> do
          lt' <- interpretLowType (dataSize ax) lt
          value' <- reflRawTerm ax value
          ptr' <- reflRawTerm ax ptr
          return $ m :< RT.Magic (M.Store lt' value' ptr')
        _ ->
          Left $ newError m "arity mismatch"
    "load" ->
      case rest of
        [lt, ptr] -> do
          lt' <- interpretLowType (dataSize ax) lt
          ptr' <- reflRawTerm ax ptr
          return $ m :< RT.Magic (M.Load lt' ptr')
        _ ->
          Left $ newError m "arity mismatch"
    "global" -> do
      case rest of
        [varName, lt] -> do
          (_, varName') <- getSymbol varName
          lt' <- interpretLowType (dataSize ax) lt
          return $ m :< RT.Magic (M.Global lt' (EN.ExternalName varName'))
        _ ->
          Left $ newError m "arity mismatch"
    "external" -> do
      let (rest', attrs) = splitAttrs rest
      case rest' of
        [] ->
          Left $ newError m "unexpected end of node, expecting: external function name"
        (extFunName : args) -> do
          (_, extFunName') <- getSymbol extFunName
          let extFunName'' = EN.ExternalName extFunName'
          args' <- mapM (reflRawTerm ax) args
          -- variadicArgs <- getRest attrs >>= mapM (reflRawTermAndLowType ax)
          variadicArgs <- undefined attrs >>= mapM (reflRawTermAndLowType ax)
          (domList, cod) <- lookupDeclEnv' m (DN.Ext extFunName'') (declEnv ax)
          return $ m :< RT.Magic (M.External domList cod extFunName'' args' variadicArgs)
    _ ->
      Left $ newError m $ "no such magic is available: " <> sym

-- getRest :: Map.HashMap T.Text Tree -> EE [(Tree, Tree)]
-- getRest attrs = do
--   case Map.lookup "rest" attrs of
--     Just t ->
--       snd <$> getArgList t
--     _ ->
--       return []

reflRawTermAndLowType :: Axis -> (Tree, Tree) -> EE (LT.LowType, RT.RawTerm)
reflRawTermAndLowType ax (e, t) = do
  e' <- reflRawTerm ax e
  t' <- interpretLowType (dataSize ax) t
  return (t', e')

reflMatch :: Axis -> Hint -> [Tree] -> EE RT.RawTerm
reflMatch ax m ts =
  case ts of
    matchHead : rest -> do
      isNoetic <- reflMatchHead matchHead
      let (args, clauses) = break isClause rest
      args' <- mapM (reflRawTerm ax) args
      clauses' <- mapM (reflPatternRow ax (length args)) clauses
      return $ m :< RT.DataElim isNoetic args' (RP.new clauses')
    [] ->
      Left $ newError m "reflMatch"

reflMatchHead :: Tree -> EE Bool
reflMatchHead t@(m :< _) =
  case t of
    _ :< Atom (AT.Symbol sym)
      | sym == "#match" ->
          return False
      | sym == "#match&" ->
          return True
    _ ->
      Left $ newError m "reflMatchHead"

isClause :: Tree -> Bool
isClause t =
  case t of
    _ :< Node ts ->
      any isArrow ts
    _ ->
      False

reflPatternRow :: Axis -> Int -> Tree -> EE (RP.RawPatternRow RT.RawTerm)
reflPatternRow ax patternSize t = do
  (m, ts) <- toNode t
  (pats, body) <- reflArrowArgs' m ts
  if length pats /= patternSize
    then
      Left $
        newError m $
          "the size of the pattern row `"
            <> T.pack (show (length pats))
            <> "` doesn't match with its input size `"
            <> T.pack (show patternSize)
            <> "`"
    else do
      patternList <- mapM reflPattern pats
      body' <- reflRawTerm ax body
      return (V.fromList patternList, body')

reflPattern :: Tree -> EE (Hint, RP.RawPattern)
reflPattern t = do
  case t of
    m :< Atom (AT.Symbol sym) -> do
      return (m, RP.Var $ Name.fromText m sym)
    m :< Node (cons : args) -> do
      (mHead, headName) <- reflName cons
      case args of
        [_ :< Atom (AT.Symbol "..")] ->
          return (m, RP.Cons headName (Left mHead))
        _ -> do
          args' <- mapM reflPattern args
          return (m, RP.Cons headName (Right args'))
    _ :< List _ -> do
      (m, ts) <- toList t
      reflPatternListIntro m ts
    m :< _ ->
      Left $ newError m $ "expected a pattern, but found:\n" <> showTree t

reflPatternListIntro :: Hint -> [Tree] -> EE (Hint, RP.RawPattern)
reflPatternListIntro m es = do
  es' <- mapM reflPattern es
  listNil <- DD.getLocatorPair m coreListNil
  listCons <- DD.getLocatorPair m coreListCons
  return $ foldListAppPat m listNil (Locator listCons) es'

foldListAppPat ::
  Hint ->
  L.Locator ->
  Name ->
  [(Hint, RP.RawPattern)] ->
  (Hint, RP.RawPattern)
foldListAppPat m listNil listCons es =
  case es of
    [] ->
      (m, RP.Var $ Locator listNil)
    e : rest -> do
      let rest' = foldListAppPat m listNil listCons rest
      (m, RP.Cons listCons (Right [e, rest']))

reflName :: Tree -> EE (Hint, Name)
reflName t = do
  (m, varText) <- getSymbol t
  case DD.getLocatorPair m varText of
    Left _ ->
      return (m, Var varText)
    Right (gl, ll) ->
      return (m, Locator (gl, ll))

reflAdmit :: Hint -> EE RT.RawTerm
reflAdmit m = do
  admit <- locatorToVarGlobal' m coreSystemAdmit
  textType <- locatorToVarGlobal' m coreText
  return $
    m
      :< RT.Annotation
        Warning
        (Annot.Type ())
        ( m
            :< RT.PiElim
              admit
              [ m :< RT.Hole,
                m :< RT.Prim (WP.Value (WPV.StaticText textType ("admit: " <> T.pack (Hint.toString m) <> "\n")))
              ]
        )

reflListIntro :: Axis -> Hint -> [Tree] -> EE RT.RawTerm
reflListIntro ax m es = do
  es' <- mapM (reflRawTerm ax) es
  listNil <- locatorToVarGlobal' m coreListNil
  listCons <- locatorToVarGlobal' m coreListCons
  return $ foldListApp m listNil listCons es'

foldListApp :: Hint -> RT.RawTerm -> RT.RawTerm -> [RT.RawTerm] -> RT.RawTerm
foldListApp m listNil listCons es =
  case es of
    [] ->
      listNil
    e : rest ->
      m :< RT.PiElim listCons [e, foldListApp m listNil listCons rest]

reflIntrospect :: Axis -> Hint -> Tree -> [Tree] -> EE RT.RawTerm
reflIntrospect ax m key clauseList = do
  (_, key') <- getSymbol key
  value <- getIntrospectiveValue ax m key'
  clauseList' <- mapM (reflIntrospectiveClause ax) clauseList
  lookupIntrospectiveClause m value clauseList'

lookupIntrospectiveClause :: Hint -> T.Text -> [(Maybe T.Text, RT.RawTerm)] -> EE RT.RawTerm
lookupIntrospectiveClause m value clauseList =
  case clauseList of
    [] ->
      Left $ newError m $ "this term doesn't support `" <> value <> "`."
    (Just key, clause) : rest
      | key == value ->
          return clause
      | otherwise ->
          lookupIntrospectiveClause m value rest
    (Nothing, clause) : _ ->
      return clause

reflIntrospectiveClause :: Axis -> Tree -> EE (Maybe T.Text, RT.RawTerm)
reflIntrospectiveClause ax t = do
  case t of
    _ :< Node [sym, arrow, body] -> do
      (_, c) <- getSymbol sym
      chunk "->" arrow
      body' <- reflRawTerm ax body
      if c /= "default"
        then return (Just c, body')
        else return (Nothing, body')
    m :< _ ->
      Left $ newError m "introspective clause"

getIntrospectiveValue :: Axis -> Hint -> T.Text -> EE T.Text
getIntrospectiveValue ax m key = do
  case key of
    "platform" -> do
      return $ Platform.reify (platform ax)
    "arch" ->
      return $ Arch.reify (Platform.arch (platform ax))
    "os" ->
      return $ OS.reify (Platform.os (platform ax))
    "build-mode" ->
      return $ BM.reify (buildMode ax)
    _ ->
      Left $ newError m $ "no such introspective value is defined: " <> key

lam :: Hint -> [RawBinder RT.RawTerm] -> RT.RawTerm -> RT.RawTerm
lam m varList e =
  m :< RT.PiIntro (LK.Normal O.Transparent) varList e

locatorToVarGlobal' :: Hint -> T.Text -> EE RT.RawTerm
locatorToVarGlobal' m text = do
  (gl, ll) <- DD.getLocatorPair m text
  return $ m :< RT.Var (Locator (gl, ll))

-- reflArgList :: Axis -> Tree -> Either Error [RawBinder RT.RawTerm]
-- reflArgList ax t = do
--   (_, ts) <- getArgList t
--   reflArgList' ax ts

-- reflArgList' :: Axis -> [(Tree, Tree)] -> Either Error [RawBinder RT.RawTerm]
-- reflArgList' ax ts = do
--   case ts of
--     [] ->
--       return []
--     (x, t) : rest -> do
--       (m, x') <- getSymbol x
--       t' <- reflRawTerm ax t
--       rest' <- reflArgList' ax rest
--       return $ (m, x', t') : rest'

-- getArgList :: Tree -> Either Error (Hint, [(Tree, Tree)])
-- getArgList tree =
--   case tree of
--     m :< List tss -> do
--       tss' <- mapM (getPairListElem m) tss
--       return (m, tss')
--     m :< _ ->
--       Left $ newError m $ "a symbol is expected, but found:\n" <> showTree tree

-- getPairListElem :: Hint -> [Tree] -> Either Error (Tree, Tree)
-- getPairListElem m ts =
--   case ts of
--     [t] ->
--       return (t, m :< Atom (AT.Symbol "_"))
--     [t1, t2] ->
--       return (t1, t2)
--     _ ->
--       Left $ newError m $ "a list of size 1 is expected, but found a list of size " <> T.pack (show (length ts))

reflArgList :: Axis -> [Tree] -> Either Error [RawBinder RT.RawTerm]
reflArgList ax ts = do
  pairs <- mapM getPairListElem ts
  reflArgList' ax pairs

reflArgList' :: Axis -> [(Tree, Tree)] -> Either Error [RawBinder RT.RawTerm]
reflArgList' ax ts = do
  case ts of
    [] ->
      return []
    (x, t) : rest -> do
      (m, x') <- getSymbol x
      t' <- reflRawTerm ax t
      rest' <- reflArgList' ax rest
      return $ (m, x', t') : rest'

-- getArgList :: Tree -> Either Error (Hint, [(Tree, Tree)])
-- getArgList tree =
--   case tree of
--     m :< List tss -> do
--       tss' <- mapM (getPairListElem m) tss
--       return (m, tss')
--     m :< _ ->
--       Left $ newError m $ "a symbol is expected, but found:\n" <> showTree tree

-- getPairListElem :: Hint -> [Tree] -> Either Error (Tree, Tree)
-- getPairListElem m ts =
--   case ts of
--     [t] ->
--       return (t, m :< Atom (AT.Symbol "_"))
--     [t1, t2] ->
--       return (t1, t2)
--     _ ->
--       Left $ newError m $ "a list of size 1 is expected, but found a list of size " <> T.pack (show (length ts))

getPairListElem :: Tree -> Either Error (Tree, Tree)
getPairListElem t =
  case t of
    m :< Atom {} ->
      return (t, m :< Atom (AT.Symbol "_"))
    _ :< Node [t1, t2] ->
      return (t1, t2)
    m :< _ ->
      Left $ newError m $ "expected a pair, found: " <> showTree t

-- case ts of
--   [t] ->
--     return (t, m :< Atom (AT.Symbol "_"))
--   [t1, t2] ->
--     return (t1, t2)
--   _ ->
--     Left $ newError m $ "a list of size 1 is expected, but found a list of size " <> T.pack (show (length ts))
