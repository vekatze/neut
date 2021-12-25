{-# LANGUAGE TemplateHaskell #-}

module Parse.WeakTerm
  ( weakTerm,
    weakTermSimple,
    weakBinder,
    weakAscription,
    ascriptionInner,
  )
where

import Codec.Binary.UTF8.String (encode)
import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (forM)
import Data.Basic
  ( EnumCase,
    EnumCaseF (EnumCaseDefault, EnumCaseLabel),
    Hint,
    Ident,
    LamKind (LamKindFix),
    Opacity (OpacityTransparent),
    asIdent,
    asText,
  )
import Data.Global (newAster, nsSep, targetPlatform)
import Data.IORef (readIORef)
import Data.Log (raiseError)
import Data.LowType
  ( Derangement (..),
    LowType
      ( LowTypeArray,
        LowTypeFloat,
        LowTypeInt,
        LowTypePointer,
        LowTypeStruct
      ),
    asLowFloat,
    asLowInt,
    getDerangementName,
    showFloatSize,
    showIntSize,
  )
import qualified Data.Text as T
import Data.WeakTerm
  ( WeakBinder,
    WeakPattern,
    WeakTerm,
    WeakTermF
      ( WeakTermCase,
        WeakTermConst,
        WeakTermDerangement,
        WeakTermEnumElim,
        WeakTermFloat,
        WeakTermIgnore,
        WeakTermInt,
        WeakTermPi,
        WeakTermPiElim,
        WeakTermPiIntro,
        WeakTermQuestion,
        WeakTermTau
      ),
    metaOf,
  )
import Parse.Core
  ( betweenParen,
    char,
    currentHint,
    float,
    integer,
    isKeyword,
    isSymbolChar,
    lam,
    lookAhead,
    many,
    many1,
    newTextualIdentFromText,
    raiseParseError,
    sepBy2,
    simpleSymbol,
    skip,
    string,
    symbol,
    symbolMaybe,
    token,
    tryPlanList,
    var,
    weakTermToWeakIdent,
    weakVar,
    weakVar',
  )

--
-- parser for WeakTerm
--

weakTerm :: IO WeakTerm
weakTerm = do
  headSymbol <- lookAhead (symbolMaybe isSymbolChar)
  case headSymbol of
    Just "lambda" ->
      weakTermPiIntro
    Just "fix" ->
      weakTermPiIntroFix
    Just "*" ->
      weakTermAster
    Just "switch" ->
      weakTermEnumElim
    Just "question" ->
      weakTermQuestion
    Just "derangement" ->
      weakTermDerangement
    Just "match" ->
      weakTermMatch
    Just "match-noetic" ->
      weakTermMatchNoetic
    Just "let" ->
      weakTermLet
    Just "let?" ->
      weakTermLetCoproduct
    Just "if" ->
      weakTermIf
    Just "idealize" ->
      weakTermIdealize
    Just "new-array" ->
      weakTermArrayIntro
    _ ->
      tryPlanList
        [ weakTermDep,
          weakTermAux
        ]

weakTermTau :: IO WeakTerm
weakTermTau = do
  m <- currentHint
  token "tau"
  return $ m :< WeakTermTau

weakTermAster :: IO WeakTerm
weakTermAster = do
  m <- currentHint
  token "?"
  newAster m

weakTermPiIntro :: IO WeakTerm
weakTermPiIntro = do
  m <- currentHint
  token "lambda"
  varList <- many weakBinder
  e <- tryPlanList [weakTermDotBind, weakTermDoEnd]
  return $ lam m varList e

weakTermDotBind :: IO WeakTerm
weakTermDotBind = do
  char '.' >> skip
  weakTerm

weakTermDoEnd :: IO WeakTerm
weakTermDoEnd = do
  token "do"
  e <- weakTerm
  token "end"
  return e

weakTermPiIntroFix :: IO WeakTerm
weakTermPiIntroFix = do
  m <- currentHint
  token "fix"
  self <- weakBinder
  varList <- many weakBinder
  e <- tryPlanList [weakTermDotBind, weakTermDoEnd]
  return $ m :< WeakTermPiIntro OpacityTransparent (LamKindFix self) varList e

weakTermAux :: IO WeakTerm
weakTermAux = do
  m <- currentHint
  xt@(_, _, e) <- weakTermToWeakIdent m weakTermSimple
  tryPlanList
    [ weakTermPi m xt,
      weakTermSigma m xt,
      weakTermApp m e
    ]

weakTermPi :: Hint -> WeakBinder -> IO WeakTerm
weakTermPi m xt = do
  xts <- many1 (token "->" >> weakTermArrowItem)
  let (_, _, cod) = last xts
  return $ m :< WeakTermPi (xt : init xts) cod

weakTermSigma :: Hint -> WeakBinder -> IO WeakTerm
weakTermSigma m xt = do
  xts <- many1 (token "*" >> weakTermArrowItem)
  toSigma m $ xt : xts

weakTermApp :: Hint -> WeakTerm -> IO WeakTerm
weakTermApp m e = do
  es <- many weakTermSimple
  if null es
    then return e
    else return $ m :< WeakTermPiElim e es

weakTermDep :: IO WeakTerm
weakTermDep = do
  m <- currentHint
  xt <- weakAscription
  tryPlanList
    [ weakTermPi m xt,
      weakTermSigma m xt
    ]

weakTermArrowItem :: IO WeakBinder
weakTermArrowItem =
  tryPlanList
    [ weakAscription,
      do
        m <- currentHint
        a <- tryPlanList [weakTermSimple >>= weakTermApp m, weakTermTau, weakTermVar]
        h <- newTextualIdentFromText "_"
        return (m, h, a)
    ]

weakTermEnumElim :: IO WeakTerm
weakTermEnumElim = do
  m <- currentHint
  token "switch"
  e <- weakTerm
  token "with"
  clauseList <- many weakTermEnumClause
  token "end"
  h <- newAster m
  return $ m :< WeakTermEnumElim (e, h) clauseList

weakTermEnumClause :: IO (EnumCase, WeakTerm)
weakTermEnumClause = do
  m <- currentHint
  token "-"
  c <- symbol
  token "->"
  body <- weakTerm
  case c of
    "default" ->
      return (m :< EnumCaseDefault, body)
    _ ->
      return (m :< EnumCaseLabel c, body)

-- question e
weakTermQuestion :: IO WeakTerm
weakTermQuestion = do
  m <- currentHint
  token "question"
  e <- weakTerm
  h <- newAster m
  return $ m :< WeakTermQuestion e h

weakTermDerangement :: IO WeakTerm
weakTermDerangement = do
  m <- currentHint
  token "derangement"
  d <- tryPlanList [weakTermDerangementNop, betweenParen weakTermDerangementKind]
  es <- many weakTermSimple
  checkDerangementArity m d (length es)
  return $ m :< WeakTermDerangement d es

checkDerangementArity :: Hint -> Derangement -> Int -> IO ()
checkDerangementArity m derangement actualArgLen = do
  let mExpectedArgLen = getDerangementArity derangement
  case mExpectedArgLen of
    Just expectedArgLen
      | expectedArgLen /= actualArgLen ->
        raiseError m $
          "the derangement `"
            <> getDerangementName derangement
            <> "` expects "
            <> T.pack (show expectedArgLen)
            <> " arguments, but found "
            <> T.pack (show actualArgLen)
            <> "."
    _ ->
      return ()

getDerangementArity :: Derangement -> Maybe Int
getDerangementArity d =
  case d of
    DerangementNop ->
      return 0
    DerangementStore _ ->
      return 2
    DerangementLoad _ ->
      return 1
    _ ->
      Nothing

weakTermDerangementNop :: IO Derangement
weakTermDerangementNop = do
  token "nop"
  return DerangementNop

weakTermDerangementKind :: IO Derangement
weakTermDerangementKind = do
  m <- currentHint
  headSymbol <- symbol
  case headSymbol of
    "nop" ->
      return DerangementNop
    "store" ->
      DerangementStore <$> lowTypeSimple
    "load" ->
      DerangementLoad <$> lowTypeSimple
    "create-array" ->
      DerangementCreateArray <$> lowTypeSimple
    "create-struct" ->
      DerangementCreateStruct <$> many lowTypeSimple
    "syscall" ->
      DerangementSyscall <$> integer
    "external" ->
      DerangementExternal <$> symbol
    _ ->
      raiseParseError m "invalid derangement kind"

-- t ::= i{n} | f{n} | pointer t | array INT t | struct t ... t
lowType :: IO LowType
lowType = do
  m <- currentHint
  headSymbol <- symbol
  case headSymbol of
    "pointer" ->
      LowTypePointer <$> lowTypeSimple
    "array" -> do
      intValue <- integer
      LowTypeArray (fromInteger intValue) <$> lowTypeSimple
    "struct" ->
      LowTypeStruct <$> many lowTypeSimple
    _
      | Just size <- asLowInt headSymbol ->
        return $ LowTypeInt size
      | Just size <- asLowFloat headSymbol ->
        return $ LowTypeFloat size
      | otherwise ->
        raiseParseError m "lowType"

lowTypeSimple :: IO LowType
lowTypeSimple =
  tryPlanList
    [ betweenParen lowType,
      lowTypeInt,
      lowTypeFloat
    ]

lowTypeInt :: IO LowType
lowTypeInt = do
  m <- currentHint
  headSymbol <- symbol
  case asLowInt headSymbol of
    Just size ->
      return $ LowTypeInt size
    Nothing ->
      raiseParseError m "lowTypeInt"

lowTypeFloat :: IO LowType
lowTypeFloat = do
  m <- currentHint
  headSymbol <- symbol
  case asLowFloat headSymbol of
    Just size ->
      return $ LowTypeFloat size
    Nothing ->
      raiseParseError m "lowTypeFloat"

weakTermMatch :: IO WeakTerm
weakTermMatch = do
  m <- currentHint
  token "match"
  e <- weakTerm
  token "with"
  clauseList <- many weakTermMatchClause
  token "end"
  return $ m :< WeakTermCase (doNotCare m) Nothing (e, doNotCare m) clauseList

weakTermMatchNoetic :: IO WeakTerm
weakTermMatchNoetic = do
  m <- currentHint
  token "match-noetic"
  e <- weakTerm
  token "with"
  s <- newAster m
  t <- newAster m
  let e' = castFromNoema s t e
  clauseList <- many weakTermMatchClause
  token "end"
  let clauseList' = map (modifyWeakPattern s) clauseList
  return $ m :< WeakTermCase (doNotCare m) (Just s) (e', doNotCare m) clauseList'

weakTermMatchClause :: IO (WeakPattern, WeakTerm)
weakTermMatchClause = do
  token "-"
  pat <- weakTermPattern
  body <- weakTerm
  return (pat, body)

modifyWeakPattern :: WeakTerm -> (WeakPattern, WeakTerm) -> (WeakPattern, WeakTerm)
modifyWeakPattern s ((m, a, xts), body) =
  ((m, a, xts), modifyWeakPatternBody s xts body)

modifyWeakPatternBody :: WeakTerm -> [WeakBinder] -> WeakTerm -> WeakTerm
modifyWeakPatternBody s xts body =
  case xts of
    [] ->
      body
    ((m, x, t) : rest) ->
      bind m (m, x, wrapWithNoema s t) (castToNoema s t (weakVar' m x)) $
        modifyWeakPatternBody s rest body

weakTermPattern :: IO WeakPattern
weakTermPattern = do
  m <- currentHint
  c <- symbol
  argList <- weakTermPatternArgument
  return (m, c, argList)

weakTermPatternArgument :: IO [WeakBinder]
weakTermPatternArgument = do
  m <- currentHint
  x <- symbol
  skip
  if x == "->"
    then return []
    else do
      tmp <- weakTermPatternArgument
      h <- newAster m
      return $ (m, asIdent x, h) : tmp

-- let x : A = e1 in e2
-- let x     = e1 in e2
weakTermLet :: IO WeakTerm
weakTermLet =
  tryPlanList [weakTermLetSigmaElim, weakTermLetNormal]

weakTermLetNormal :: IO WeakTerm
weakTermLetNormal = do
  m <- currentHint
  token "let"
  x <- weakTermLetVar
  char '=' >> skip
  e1 <- weakTerm
  token "in"
  e2 <- weakTerm
  t1 <- newAster m
  resultType <- newAster m
  return $
    m
      :< WeakTermPiElim
        (weakVar m "identity.bind")
        [ t1,
          resultType,
          e1,
          lam m [x] e2
        ]

weakTermSigmaElimVar :: IO WeakBinder
weakTermSigmaElimVar =
  tryPlanList [ascriptionInner, weakAscription']

-- let (x1 : A1, ..., xn : An) = e1 in e2
weakTermLetSigmaElim :: IO WeakTerm
weakTermLetSigmaElim = do
  m <- currentHint
  token "let"
  xts <- betweenParen $ sepBy2 weakTermSigmaElimVar (char ',' >> skip)
  token "="
  e1 <- weakTerm
  token "in"
  e2 <- weakTerm
  resultType <- newAster m
  return $
    m
      :< WeakTermPiElim
        e1
        [ resultType,
          lam m xts e2
        ]

-- let? x : A = e1 in e2
-- let? x     = e1 in e2
weakTermLetCoproduct :: IO WeakTerm
weakTermLetCoproduct = do
  m <- currentHint
  token "let?"
  x <- weakTermLetVar
  char '=' >> skip
  e1 <- weakTerm
  token "in"
  e2 <- weakTerm
  err <- newTextualIdentFromText "err"
  typeOfLeft <- newAster m
  typeOfRight <- newAster m
  let sumLeft = "sum.left"
  let sumRight = "sum.right"
  let sumLeftVar = asIdent "sum.left"
  return $
    m
      :< WeakTermCase
        (doNotCare m)
        Nothing
        (e1, doNotCare m)
        [ ( (m, sumLeft, [(m, err, typeOfLeft)]),
            m :< WeakTermPiElim (weakVar' m sumLeftVar) [typeOfLeft, typeOfRight, weakVar' m err]
          ),
          ( (m, sumRight, [x]),
            e2
          )
        ]

weakTermLetVar :: IO WeakBinder
weakTermLetVar = do
  m <- currentHint
  tryPlanList
    [ do
        x <- simpleSymbol
        char ':'
        skip
        a <- weakTerm
        return (m, asIdent x, a),
      do
        x <- simpleSymbol
        h <- newAster m
        return (m, asIdent x, h)
    ]

weakTermIf :: IO WeakTerm
weakTermIf = do
  m <- currentHint
  token "if"
  ifCond <- weakTerm
  token "then"
  ifBody <- weakTerm
  elseIfList <- many $ do
    token "else-if"
    elseIfCond <- weakTerm
    token "then"
    elseIfBody <- weakTerm
    return (elseIfCond, elseIfBody)
  token "else"
  elseBody <- weakTerm
  token "end"
  foldIf m ifCond ifBody elseIfList elseBody

foldIf :: Hint -> WeakTerm -> WeakTerm -> [(WeakTerm, WeakTerm)] -> WeakTerm -> IO WeakTerm
foldIf m ifCond ifBody elseIfList elseBody =
  case elseIfList of
    [] -> do
      h <- newAster m
      return $
        m
          :< WeakTermEnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel "bool.true", ifBody),
              (m :< EnumCaseLabel "bool.false", elseBody)
            ]
    ((elseIfCond, elseIfBody) : rest) -> do
      cont <- foldIf m elseIfCond elseIfBody rest elseBody
      h <- newAster m
      return $
        m
          :< WeakTermEnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel "bool.true", ifBody),
              (m :< EnumCaseLabel "bool.false", cont)
            ]

-- (e1, ..., en) (n >= 2)
weakTermSigmaIntro :: IO WeakTerm
weakTermSigmaIntro = do
  m <- currentHint
  betweenParen $ do
    es <- sepBy2 weakTerm (char ',' >> skip)
    xts <- forM es $ \_ -> do
      x <- newTextualIdentFromText "_"
      t <- newAster m
      return (m, x, t)
    sigVar <- newTextualIdentFromText "sigvar"
    k <- newTextualIdentFromText "sig-k"
    return $
      lam
        m
        [ (m, sigVar, m :< WeakTermTau),
          (m, k, m :< WeakTermPi xts (weakVar' m sigVar))
        ]
        (m :< WeakTermPiElim (weakVar' m k) es)

weakTermIdealize :: IO WeakTerm
weakTermIdealize = do
  m <- currentHint
  token "idealize"
  varList <- many var
  let varList' = fmap (fmap asIdent) varList
  token "over"
  mSubject <- currentHint
  subject <- simpleSymbol
  token "in"
  e <- weakTerm
  resultType <- newAster m
  let subjectTerm = weakVar mSubject subject
  ts <- mapM (\(mx, _) -> newAster mx) varList
  return $
    m
      :< WeakTermPiElim
        (weakVar m "idea.run")
        [ resultType,
          lam
            m
            [(mSubject, asIdent subject, weakVar m "subject")]
            (castLet subjectTerm (zip varList' ts) e)
        ]

castLet :: WeakTerm -> [((Hint, Ident), WeakTerm)] -> WeakTerm -> WeakTerm
castLet subject xts cont =
  case xts of
    [] ->
      cont
    ((m, x), t) : rest ->
      bind m (m, x, wrapWithNoema subject t) (castToNoema subject t (m :< WeakTermIgnore (weakVar' m x))) $
        castLet subject rest cont

weakTermArrayIntro :: IO WeakTerm
weakTermArrayIntro = do
  m <- currentHint
  token "new-array"
  t <- lowTypeSimple
  es <- many weakTermSimple
  arr <- newTextualIdentFromText "arr"
  ptr <- newTextualIdentFromText "ptr"
  h1 <- newTextualIdentFromText "_"
  h2 <- newTextualIdentFromText "_"
  let ptrType = weakVar m "unsafe.pointer"
  let topType = weakVar m "top"
  arrText <- lowTypeToArrayKindText m t
  let arrName = arrText <> "-array" -- e.g. i8-array
  t' <- lowTypeToWeakTerm m t
  es' <- mapM (annotate t') es
  return $
    bind m (m, arr, ptrType) (m :< WeakTermDerangement (DerangementCreateArray t) es') $
      bind m (m, ptr, ptrType) (m :< WeakTermPiElim (weakVar m "memory.allocate") [intTerm m 16]) $
        bind m (m, h1, topType) (m :< WeakTermPiElim (weakVar m "memory.store-i64-with-index") [weakVar m (asText ptr), intTerm m 0, intTerm m (toInteger (length es))]) $
          bind
            m
            (m, h2, topType)
            (m :< WeakTermPiElim (weakVar m "memory.store-pointer-with-index") [weakVar m (asText ptr), intTerm m 1, weakVar m (asText arr)])
            (m :< WeakTermPiElim (weakVar m "unsafe.cast") [weakVar m "unsafe.pointer", weakVar m arrName, weakVar m (asText ptr)])

lowTypeToWeakTerm :: Hint -> LowType -> IO WeakTerm
lowTypeToWeakTerm m t =
  case t of
    LowTypeInt s ->
      return (m :< WeakTermConst (showIntSize s))
    LowTypeFloat s ->
      return (m :< WeakTermConst (showFloatSize s))
    _ ->
      raiseParseError m "invalid argument passed to lowTypeToType"

annotate :: WeakTerm -> WeakTerm -> IO WeakTerm
annotate t e = do
  let m = metaOf e
  h <- newTextualIdentFromText "_"
  return $ bind m (m, h, t) e $ weakVar m (asText h)

lowTypeToArrayKindText :: Hint -> LowType -> IO T.Text
lowTypeToArrayKindText m t =
  case t of
    LowTypeInt size ->
      return $ showIntSize size
    LowTypeFloat size ->
      return $ showFloatSize size
    _ ->
      raiseParseError m "unsupported array kind"

intTerm :: Hint -> Integer -> WeakTerm
intTerm m i =
  m :< WeakTermInt (m :< WeakTermConst "i64") i

bind :: Hint -> WeakBinder -> WeakTerm -> WeakTerm -> WeakTerm
bind m mxt e cont =
  m :< WeakTermPiElim (lam m [mxt] cont) [e]

weakTermAdmit :: IO WeakTerm
weakTermAdmit = do
  m <- currentHint
  token "admit"
  h <- newAster m
  return $
    m
      :< WeakTermPiElim
        (weakVar m "os.exit")
        [ h,
          m :< WeakTermInt (m :< WeakTermConst "i64") 1
        ]

weakTermAdmitQuestion :: IO WeakTerm
weakTermAdmitQuestion = do
  m <- currentHint
  token "?admit"
  h <- newAster m
  return $
    m
      :< WeakTermQuestion
        ( m
            :< WeakTermPiElim
              (weakVar m "os.exit")
              [ h,
                m :< WeakTermInt (m :< WeakTermConst "i64") 1
              ]
        )
        h

--
-- term-related helper functions
--

weakTermSimple :: IO WeakTerm
weakTermSimple =
  tryPlanList
    [ weakTermSigmaIntro,
      betweenParen weakTerm,
      weakTermTau,
      weakTermAster,
      weakTermString,
      weakTermInteger,
      weakTermFloat,
      weakTermBuiltin,
      weakTermAdmitQuestion,
      weakTermAdmit,
      weakTermVar
    ]

weakBinder :: IO WeakBinder
weakBinder =
  tryPlanList
    [ weakAscription,
      weakAscription'
    ]

ascriptionInner :: IO WeakBinder
ascriptionInner = do
  m <- currentHint
  x <- symbol
  char ':' >> skip
  a <- weakTerm
  return (m, asIdent x, a)

weakAscription :: IO WeakBinder
weakAscription =
  betweenParen ascriptionInner

weakAscription' :: IO WeakBinder
weakAscription' = do
  (m, x) <- weakSimpleIdent
  h <- newAster m
  return (m, x, h)

weakSimpleIdent :: IO (Hint, Ident)
weakSimpleIdent = do
  m <- currentHint
  x <- simpleSymbol
  if isKeyword x
    then raiseParseError m $ "found a keyword `" <> x <> "`, expecting a variable"
    else return (m, asIdent x)

weakTermBuiltin :: IO WeakTerm
weakTermBuiltin = do
  m <- currentHint
  x <- symbol
  case x of
    "target-platform" -> do
      target <- readIORef targetPlatform
      return (weakVar m ("target" <> nsSep <> T.pack target))
    _ ->
      raiseParseError m $ "no such builtin constant: " <> x

weakTermVar :: IO WeakTerm
weakTermVar = do
  (m, x) <- var
  return (weakVar m x)

weakTermString :: IO WeakTerm
weakTermString = do
  m <- currentHint
  s <- string
  let i8s = encode $ T.unpack s
  let len = toInteger $ length i8s
  let i8s' = map (\x -> m :< WeakTermInt (m :< WeakTermConst "i8") (toInteger x)) i8s
  return $
    m
      :< WeakTermPiElim
        (weakVar m "unsafe.create-new-string")
        [ m :< WeakTermInt (m :< WeakTermConst "i64") len,
          m
            :< WeakTermDerangement
              (DerangementCreateArray (LowTypeInt 8))
              i8s'
        ]

weakTermInteger :: IO WeakTerm
weakTermInteger = do
  m <- currentHint
  intValue <- integer
  h <- newAster m
  return $ m :< WeakTermInt h intValue

weakTermFloat :: IO WeakTerm
weakTermFloat = do
  m <- currentHint
  floatValue <- float
  h <- newAster m
  return $ m :< WeakTermFloat h floatValue

toSigma :: Hint -> [WeakBinder] -> IO WeakTerm
toSigma m xts = do
  sigVar <- newTextualIdentFromText "sig"
  h <- newTextualIdentFromText "_"
  return $
    m
      :< WeakTermPi
        [ (m, sigVar, m :< WeakTermTau),
          (m, h, m :< WeakTermPi xts (weakVar' m sigVar))
        ]
        (weakVar' m sigVar)

castFromNoema :: WeakTerm -> WeakTerm -> WeakTerm -> WeakTerm
castFromNoema subject baseType tree = do
  let m = metaOf tree
  m
    :< WeakTermPiElim
      (weakVar m "unsafe.cast")
      [ wrapWithNoema subject baseType,
        baseType,
        tree
      ]

castToNoema :: WeakTerm -> WeakTerm -> WeakTerm -> WeakTerm
castToNoema subject baseType tree = do
  let m = metaOf tree
  m
    :< WeakTermPiElim
      (weakVar m "unsafe.cast")
      [ baseType,
        wrapWithNoema subject baseType,
        tree
      ]

wrapWithNoema :: WeakTerm -> WeakTerm -> WeakTerm
wrapWithNoema subject baseType = do
  let m = metaOf baseType
  m :< WeakTermPiElim (weakVar m "noema") [subject, baseType]

doNotCare :: Hint -> WeakTerm
doNotCare m =
  m :< WeakTermTau
