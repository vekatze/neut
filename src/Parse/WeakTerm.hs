module Parse.WeakTerm
  ( weakTerm,
    weakTermSimple,
    weakBinder,
    weakAscription,
    parseTopDefInfo,
    unsnoc,
  )
where

import Codec.Binary.UTF8.String (encode)
import Control.Comonad.Cofree (Cofree (..))
import Data.Basic
  ( BinderF,
    EnumCase,
    EnumCaseF (EnumCaseDefault, EnumCaseLabel),
    Hint,
    Ident,
    LamKindF (LamKindFix, LamKindNormal),
    PatternF,
    asIdent,
    asText,
  )
import Data.Global (constBoolFalse, constBoolTrue, newAster, outputError, targetArchRef, targetOSRef, targetPlatformRef, unsafePtr)
import Data.IORef (readIORef)
import Data.List (foldl')
import Data.Log (raiseError)
import Data.LowType
  ( Derangement (..),
    LowType (..),
    asLowFloat,
    asLowInt,
    showFloatSize,
    showIntSize,
  )
import qualified Data.Text as T
import Data.WeakTerm
  ( DefInfo,
    TopDefInfo,
    WeakTerm,
    WeakTermF (..),
    metaOf,
  )
import Parse.Core
  ( currentHint,
    isKeyword,
    isSymbolChar,
    lam,
    lookAhead,
    newTextualIdentFromText,
    parseArgList,
    parseArgList2,
    parseAsBlock,
    parseBetweenBracket,
    parseBetweenParen,
    parseByPredicate,
    parseChar,
    parseDefiniteDescription,
    parseDoBlock,
    parseFloat,
    parseImpArgList,
    parseInBlock,
    parseInteger,
    parseMany,
    parseManyList,
    parseString,
    parseSymbol,
    parseToken,
    parseVar,
    raiseParseError,
    sepBy2,
    skip,
    tryPlanList,
    weakVar,
    weakVar',
  )

--
-- parser for WeakTerm
--

weakTerm :: IO WeakTerm
weakTerm = do
  headSymbol <- lookAhead (parseByPredicate isSymbolChar)
  case headSymbol of
    Just "lambda" ->
      weakTermPiIntro
    Just "define" ->
      weakTermPiIntroDef
    Just "*" ->
      weakTermAster
    Just "switch" ->
      weakTermEnumElim
    Just "introspect" ->
      weakTermIntrospect
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
    Just headSymbolText
      | T.head headSymbolText == '&' -> do
        weakTermNoema
    _ ->
      tryPlanList
        [ weakTermPiArrow,
          weakTermSigma,
          weakTermPiElim,
          weakTermPiElimInv
        ]
        weakTermSimple

weakTermTau :: IO WeakTerm
weakTermTau = do
  m <- currentHint
  parseToken "tau"
  return $ m :< WeakTermTau

weakTermAster :: IO WeakTerm
weakTermAster = do
  m <- currentHint
  parseToken "?"
  newAster m

weakTermPiArrow :: IO WeakTerm
weakTermPiArrow = do
  m <- currentHint
  domList <- parseArgList $ tryPlanList [weakAscription] typeWithoutIdent
  parseToken "->"
  cod <- weakTerm
  return $ m :< WeakTermPi domList cod

weakTermPiIntro :: IO WeakTerm
weakTermPiIntro = do
  m <- currentHint
  parseToken "lambda"
  varList <- parseArgList weakBinder
  e <- tryPlanList [weakTermDotBind] $ parseDoBlock weakTerm
  return $ lam m varList e

weakTermDotBind :: IO WeakTerm
weakTermDotBind = do
  parseChar '.' >> skip
  weakTerm

parseDefInfo :: IO DefInfo
parseDefInfo = do
  functionVar <- parseVar
  domInfoList <- parseArgList weakBinder
  parseChar ':' >> skip
  codType <- weakTerm
  e <- parseAsBlock weakTerm
  return (functionVar, domInfoList, codType, e)

parseTopDefInfo :: IO TopDefInfo
parseTopDefInfo = do
  functionVar <- parseVar
  skip
  impDomInfoList <- parseImpArgList weakBinder
  domInfoList <- parseArgList weakBinder
  parseChar ':' >> skip
  codType <- weakTerm
  e <- parseAsBlock weakTerm
  return (functionVar, impDomInfoList, domInfoList, codType, e)

-- define name(x1: A1, ..., xn: An)[: A] as e end
weakTermPiIntroDef :: IO WeakTerm
weakTermPiIntroDef = do
  m <- currentHint
  parseToken "define"
  ((mFun, functionName), domBinderList, codType, e) <- parseDefInfo
  let piType = mFun :< WeakTermPi domBinderList codType
  return $ m :< WeakTermPiIntro (LamKindFix (mFun, asIdent functionName, piType)) domBinderList e

weakTermSigma :: IO WeakTerm
weakTermSigma = do
  m <- currentHint
  xts <- sepBy2 (parseToken "*") weakTermSigmaItem
  return $ m :< WeakTermSigma xts

weakTermSigmaItem :: IO (BinderF WeakTerm)
weakTermSigmaItem =
  tryPlanList
    [parseBetweenParen weakAscription]
    $ do
      m <- currentHint
      a <- tryPlanList [weakTermPiElim, weakTermTau] weakTermVar
      h <- newTextualIdentFromText "_"
      return (m, h, a)

weakTermDefiniteDescription :: IO WeakTerm
weakTermDefiniteDescription = do
  (m, x) <- parseDefiniteDescription
  -- using VarGlobal instead of Var for better performance.
  -- cf. Parse.Discern
  return $ m :< WeakTermVarGlobal x

weakTermEnumElim :: IO WeakTerm
weakTermEnumElim = do
  m <- currentHint
  parseToken "switch"
  e <- weakTerm
  parseToken "with"
  clauseList <- parseMany weakTermEnumClause
  parseToken "end"
  h <- newAster m
  return $ m :< WeakTermEnumElim (e, h) clauseList

weakTermEnumClause :: IO (EnumCase, WeakTerm)
weakTermEnumClause = do
  m <- currentHint
  parseToken "-"
  c <- parseSymbol
  parseToken "->"
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
  parseToken "question"
  e <- weakTerm
  h <- newAster m
  return $ m :< WeakTermQuestion e h

weakTermDerangement :: IO WeakTerm
weakTermDerangement = do
  m <- currentHint
  parseToken "derangement"
  headSymbol <- parseSymbol
  parseBetweenParen $ do
    case headSymbol of
      "cast" -> do
        castFrom <- weakTerm
        castTo <- parseChar ',' >> skip >> weakTerm
        value <- parseChar ',' >> skip >> weakTerm
        return $ m :< WeakTermDerangement (DerangementCast castFrom castTo value)
      "store" -> do
        lt <- lowType
        pointer <- parseChar ',' >> skip >> weakTerm
        value <- parseChar ',' >> skip >> weakTerm
        return $ m :< WeakTermDerangement (DerangementStore lt pointer value)
      "load" -> do
        lt <- lowType
        pointer <- parseChar ',' >> skip >> weakTerm
        return $ m :< WeakTermDerangement (DerangementLoad lt pointer)
      "syscall" -> do
        syscallNum <- parseInteger
        es <- parseMany (parseChar ',' >> skip >> weakTerm)
        return $ m :< WeakTermDerangement (DerangementSyscall syscallNum es)
      "external" -> do
        extFunName <- parseSymbol
        es <- parseMany (parseChar ',' >> skip >> weakTerm)
        return $ m :< WeakTermDerangement (DerangementExternal extFunName es)
      "create-array" -> do
        lt <- lowType
        es <- parseMany (parseChar ',' >> skip >> weakTerm)
        return $ m :< WeakTermDerangement (DerangementCreateArray lt es)
      _ ->
        raiseError m $ "no such derangement is defined: " <> headSymbol

-- t ::= i{n} | f{n} | pointer t | array INT t | struct t ... t
lowType :: IO LowType
lowType = do
  m <- currentHint
  headSymbol <- parseSymbol
  case headSymbol of
    "pointer" ->
      LowTypePointer <$> lowTypeSimple
    "array" -> do
      intValue <- parseInteger
      LowTypeArray (fromInteger intValue) <$> lowTypeSimple
    "struct" ->
      LowTypeStruct <$> parseMany lowTypeSimple
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
    [ parseBetweenParen lowType,
      lowTypeInt
    ]
    lowTypeFloat

lowTypeInt :: IO LowType
lowTypeInt = do
  m <- currentHint
  headSymbol <- parseSymbol
  case asLowInt headSymbol of
    Just size ->
      return $ LowTypeInt size
    Nothing ->
      raiseParseError m "lowTypeInt"

lowTypeFloat :: IO LowType
lowTypeFloat = do
  m <- currentHint
  headSymbol <- parseSymbol
  case asLowFloat headSymbol of
    Just size ->
      return $ LowTypeFloat size
    Nothing ->
      raiseParseError m "lowTypeFloat"

weakTermMatch :: IO WeakTerm
weakTermMatch = do
  m <- currentHint
  parseToken "match"
  e <- weakTerm
  clauseList <- parseInBlock "with" $ parseManyList weakTermMatchClause
  return $ m :< WeakTermMatch Nothing (e, doNotCare m) clauseList

weakTermMatchNoetic :: IO WeakTerm
weakTermMatchNoetic = do
  m <- currentHint
  parseToken "match-noetic"
  e <- weakTerm
  parseToken "with"
  s <- newAster m
  t <- newAster m
  let e' = castFromNoema s t e
  clauseList <- parseManyList weakTermMatchClause
  parseToken "end"
  let clauseList' = map (modifyWeakPattern s) clauseList
  return $ m :< WeakTermMatch (Just s) (e', doNotCare m) clauseList'

weakTermMatchClause :: IO (PatternF WeakTerm, WeakTerm)
weakTermMatchClause = do
  pat <- weakTermPattern
  parseToken "->"
  body <- weakTerm
  return (pat, body)

modifyWeakPattern :: WeakTerm -> (PatternF WeakTerm, WeakTerm) -> (PatternF WeakTerm, WeakTerm)
modifyWeakPattern s ((m, a, xts), body) =
  ((m, a, xts), modifyWeakPatternBody s xts body)

modifyWeakPatternBody :: WeakTerm -> [BinderF WeakTerm] -> WeakTerm -> WeakTerm
modifyWeakPatternBody s xts body =
  case xts of
    [] ->
      body
    ((m, x, t) : rest) ->
      bind (m, x, wrapWithNoema s t) (castToNoema s t (weakVar' m x)) $
        modifyWeakPatternBody s rest body

weakTermPattern :: IO (PatternF WeakTerm)
weakTermPattern = do
  m <- currentHint
  c <- parseSymbol
  patArgs <- parseArgList weakBinder
  return (m, c, patArgs)

-- let x : A = e1 in e2
-- let x     = e1 in e2
weakTermLet :: IO WeakTerm
weakTermLet =
  tryPlanList [weakTermLetSigmaElim] weakTermLetNormal

weakTermLetNormal :: IO WeakTerm
weakTermLetNormal = do
  m <- currentHint
  parseToken "let"
  x <- weakTermLetVar
  parseChar '=' >> skip
  e1 <- weakTerm
  parseToken "in"
  e2 <- weakTerm
  t1 <- newAster m
  resultType <- newAster m
  return $
    m
      :< WeakTermPiElim
        (m :< WeakTermVarGlobal "core.identity::bind")
        [ t1,
          resultType,
          e1,
          lam m [x] e2
        ]

-- let (x1 : A1, ..., xn : An) = e1 in e2
weakTermLetSigmaElim :: IO WeakTerm
weakTermLetSigmaElim = do
  m <- currentHint
  parseToken "let"
  xts <- parseArgList2 weakBinder
  parseToken "="
  e1 <- weakTerm
  parseToken "in"
  e2 <- weakTerm
  return $ m :< WeakTermSigmaElim xts e1 e2

-- let? x : A = e1 in e2
-- let? x     = e1 in e2
weakTermLetCoproduct :: IO WeakTerm
weakTermLetCoproduct = do
  m <- currentHint
  parseToken "let?"
  x <- weakTermLetVar
  parseChar '=' >> skip
  e1 <- weakTerm
  parseToken "in"
  e2 <- weakTerm
  err <- newTextualIdentFromText "err"
  typeOfLeft <- newAster m
  typeOfRight <- newAster m
  let sumLeft = "sum.left"
  let sumRight = "sum.right"
  let sumLeftVar = asIdent "sum.left"
  return $
    m
      :< WeakTermMatch
        Nothing
        (e1, doNotCare m)
        [ ( (m, sumLeft, [(m, err, typeOfLeft)]),
            m :< WeakTermPiElim (weakVar' m sumLeftVar) [typeOfLeft, typeOfRight, weakVar' m err]
          ),
          ( (m, sumRight, [x]),
            e2
          )
        ]

weakTermLetVar :: IO (BinderF WeakTerm)
weakTermLetVar = do
  m <- currentHint
  tryPlanList
    [ do
        x <- parseSymbol
        parseChar ':'
        skip
        a <- weakTerm
        return (m, asIdent x, a)
    ]
    $ do
      x <- parseSymbol
      h <- newAster m
      return (m, asIdent x, h)

weakTermIf :: IO WeakTerm
weakTermIf = do
  m <- currentHint
  parseToken "if"
  ifCond <- weakTerm
  parseToken "then"
  ifBody <- weakTerm
  elseIfList <- parseMany $ do
    parseToken "else-if"
    elseIfCond <- weakTerm
    parseToken "then"
    elseIfBody <- weakTerm
    return (elseIfCond, elseIfBody)
  parseToken "else"
  elseBody <- weakTerm
  parseToken "end"
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
            [ (m :< EnumCaseLabel constBoolTrue, ifBody),
              (m :< EnumCaseLabel constBoolFalse, elseBody)
            ]
    ((elseIfCond, elseIfBody) : rest) -> do
      cont <- foldIf m elseIfCond elseIfBody rest elseBody
      h <- newAster m
      return $
        m
          :< WeakTermEnumElim
            (ifCond, h)
            [ (m :< EnumCaseLabel constBoolTrue, ifBody),
              (m :< EnumCaseLabel constBoolFalse, cont)
            ]

-- (e1, ..., en) (n >= 2)
weakTermSigmaIntro :: IO WeakTerm
weakTermSigmaIntro = do
  m <- currentHint
  es <- parseArgList2 weakTerm
  return $ m :< WeakTermSigmaIntro es

weakTermNoema :: IO WeakTerm
weakTermNoema = do
  m <- currentHint
  parseChar '&'
  subject <- asIdent <$> parseSymbol
  t <- weakTerm
  return $ m :< WeakTermNoema (m :< WeakTermVar subject) t

weakTermIdealize :: IO WeakTerm
weakTermIdealize = do
  m <- currentHint
  parseToken "idealize"
  varList <- parseMany parseVar
  let varList' = fmap (fmap asIdent) varList
  parseToken "over"
  subject <- asIdent <$> parseSymbol
  e <- parseDoBlock weakTerm
  ts <- mapM (\(mx, _) -> newAster mx) varList
  return $ m :< WeakTermNoemaElim subject (castLet subject (zip varList' ts) e)

castLet :: Ident -> [((Hint, Ident), WeakTerm)] -> WeakTerm -> WeakTerm
castLet subject xts cont =
  case xts of
    [] ->
      cont
    ((m, x), t) : rest ->
      bind (m, x, t) (m :< WeakTermNoemaIntro subject (weakVar' m x)) $ castLet subject rest cont

weakTermArrayIntro :: IO WeakTerm
weakTermArrayIntro = do
  m <- currentHint
  parseToken "new-array"
  t <- lowTypeSimple
  es <- parseMany weakTermSimple
  arr <- newTextualIdentFromText "arr"
  ptr <- newTextualIdentFromText "ptr"
  h1 <- newTextualIdentFromText "_"
  h2 <- newTextualIdentFromText "_"
  let ptrType = weakVar m unsafePtr
  let topType = weakVar m "top"
  arrText <- lowTypeToArrayKindText m t
  let arrName = arrText <> "-array" -- e.g. i8-array
  t' <- lowTypeToWeakTerm m t
  es' <- mapM (annotate t') es
  return $
    bind (m, arr, ptrType) (m :< WeakTermDerangement (DerangementCreateArray t es')) $
      bind (m, ptr, ptrType) (m :< WeakTermPiElim (weakVar m "memory.allocate") [intTerm m 16]) $
        bind (m, h1, topType) (m :< WeakTermPiElim (weakVar m "memory.store-i64-with-index") [weakVar m (asText ptr), intTerm m 0, intTerm m (toInteger (length es))]) $
          bind
            (m, h2, topType)
            (m :< WeakTermPiElim (weakVar m "memory.store-pointer-with-index") [weakVar m (asText ptr), intTerm m 1, weakVar m (asText arr)])
            (m :< WeakTermDerangement (DerangementCast (weakVar m unsafePtr) (weakVar m arrName) (weakVar m (asText ptr))))

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
  return $ bind (m, h, t) e $ weakVar m (asText h)

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

bind :: BinderF WeakTerm -> WeakTerm -> WeakTerm -> WeakTerm
bind mxt@(m, _, _) e cont =
  m :< WeakTermPiElim (lam m [mxt] cont) [e]

weakTermAdmit :: IO WeakTerm
weakTermAdmit = do
  m <- currentHint
  parseToken "admit"
  h <- newAster m
  return $
    m
      :< WeakTermPiElim
        (weakVar m "core.os.exit")
        [ h,
          m :< WeakTermInt (m :< WeakTermConst "i64") 1
        ]

weakTermAdmitQuestion :: IO WeakTerm
weakTermAdmitQuestion = do
  m <- currentHint
  parseToken "?admit"
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

weakTermPiElim :: IO WeakTerm
weakTermPiElim = do
  m <- currentHint
  e <- weakTermSimple
  impArgs <- parseImpArgList weakTerm
  es <- parseArgList weakTerm
  ess <- parseMany $ parseArgList weakTerm
  if null impArgs
    then return $ foldl' (\base args -> m :< WeakTermPiElim base args) e $ es : ess
    else do
      f <- newTextualIdentFromText "func"
      h <- newAster m
      return $
        m
          :< WeakTermPiElim
            ( m
                :< WeakTermPiIntro
                  LamKindNormal
                  [(m, f, h)]
                  ( foldl'
                      (\base args -> m :< WeakTermPiElim base args)
                      (m :< WeakTermVar f)
                      ((impArgs ++ es) : ess)
                  )
            )
            [e]

weakTermPiElimInv :: IO WeakTerm
weakTermPiElimInv = do
  m <- currentHint
  e <- weakTermSimple
  f <- parseBetweenBracket weakTerm
  fs <- parseMany $ parseBetweenBracket weakTerm
  return $ foldl' (\base func -> m :< WeakTermPiElim func [base]) e $ f : fs

--
-- term-related helper functions
--

weakTermSimple :: IO WeakTerm
weakTermSimple =
  tryPlanList
    [ weakTermSigmaIntro,
      parseBetweenParen weakTerm,
      weakTermDefiniteDescription,
      weakTermTau,
      weakTermAster,
      weakTermString,
      weakTermInteger,
      weakTermFloat,
      weakTermAdmitQuestion,
      weakTermAdmit
    ]
    weakTermVar

weakBinder :: IO (BinderF WeakTerm)
weakBinder =
  tryPlanList
    [ weakAscription
    ]
    weakAscription'

weakAscription :: IO (BinderF WeakTerm)
weakAscription = do
  m <- currentHint
  x <- parseSymbol
  parseChar ':' >> skip
  a <- weakTerm
  return (m, asIdent x, a)

typeWithoutIdent :: IO (BinderF WeakTerm)
typeWithoutIdent = do
  m <- currentHint
  x <- newTextualIdentFromText "_"
  t <- weakTerm
  return (m, x, t)

weakAscription' :: IO (BinderF WeakTerm)
weakAscription' = do
  (m, x) <- weakSimpleIdent
  h <- newAster m
  return (m, x, h)

weakSimpleIdent :: IO (Hint, Ident)
weakSimpleIdent = do
  m <- currentHint
  x <- parseSymbol
  if isKeyword x
    then raiseParseError m $ "found a keyword `" <> x <> "`, expecting a variable"
    else return (m, asIdent x)

weakTermIntrospect :: IO WeakTerm
weakTermIntrospect = do
  m <- currentHint
  parseToken "introspect"
  key <- parseSymbol
  value <- getIntrospectiveValue m key
  parseToken "with"
  clauseList <- parseMany weakTermIntrospectiveClause
  parseToken "end"
  case lookup value clauseList of
    Just clause ->
      return clause
    Nothing -> do
      outputError m $ "`" <> value <> "` is not supported here"

weakTermIntrospectiveClause :: IO (T.Text, WeakTerm)
weakTermIntrospectiveClause = do
  parseToken "-"
  c <- parseSymbol
  parseToken "->"
  body <- weakTerm
  return (c, body)

getIntrospectiveValue :: Hint -> T.Text -> IO T.Text
getIntrospectiveValue m key =
  case key of
    "target-platform" -> do
      T.pack <$> readIORef targetPlatformRef
    "target-arch" ->
      T.pack <$> readIORef targetArchRef
    "target-os" ->
      T.pack <$> readIORef targetOSRef
    _ ->
      raiseError m $ "no such introspective value is defined: " <> key

weakTermVar :: IO WeakTerm
weakTermVar = do
  (m, x) <- parseVar
  return (weakVar m x)

weakTermString :: IO WeakTerm
weakTermString = do
  m <- currentHint
  s <- parseString
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
              (DerangementCreateArray (LowTypeInt 8) i8s')
        ]

weakTermInteger :: IO WeakTerm
weakTermInteger = do
  m <- currentHint
  intValue <- parseInteger
  h <- newAster m
  return $ m :< WeakTermInt h intValue

weakTermFloat :: IO WeakTerm
weakTermFloat = do
  m <- currentHint
  floatValue <- parseFloat
  h <- newAster m
  return $ m :< WeakTermFloat h floatValue

castFromNoema :: WeakTerm -> WeakTerm -> WeakTerm -> WeakTerm
castFromNoema subject baseType tree = do
  let m = metaOf tree
  m :< WeakTermDerangement (DerangementCast (wrapWithNoema subject baseType) baseType tree)

castToNoema :: WeakTerm -> WeakTerm -> WeakTerm -> WeakTerm
castToNoema subject baseType tree = do
  let m = metaOf tree
  m :< WeakTermDerangement (DerangementCast baseType (wrapWithNoema subject baseType) tree)

wrapWithNoema :: WeakTerm -> WeakTerm -> WeakTerm
wrapWithNoema subject baseType = do
  let m = metaOf baseType
  m :< WeakTermNoema subject baseType

doNotCare :: Hint -> WeakTerm
doNotCare m =
  m :< WeakTermTau

unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  foldr go Nothing
  where
    go x acc =
      case acc of
        Nothing ->
          Just ([], x)
        Just (ys, y) ->
          Just (x : ys, y)
