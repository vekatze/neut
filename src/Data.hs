{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Data where

import           Control.Comonad
import           Control.Comonad.Cofree

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Text.Show.Deriving

import           System.IO.Unsafe

import           Data.IORef
import           Data.List

import qualified Text.Show.Pretty           as Pr

type Identifier = String

newtype Meta = Meta
  { ident :: Identifier
  } deriving (Show, Eq)

-- (undetermined) level of universe
data WeakLevel
  = WeakLevelFixed Int
  | WeakLevelHole Identifier
  deriving (Show, Eq)

type Level = Int

-- S-expression
-- the "F" stands for "Functor"
data TreeF a
  = TreeAtom Identifier
  | TreeNode [a]

deriving instance Show a => Show (TreeF a)

deriving instance Functor TreeF

$(deriveShow1 ''TreeF)

type Tree = Cofree TreeF Meta

recurM :: (Monad m) => (Tree -> m Tree) -> Tree -> m Tree
recurM f (meta :< TreeAtom s) = f (meta :< TreeAtom s)
recurM f (meta :< TreeNode tis) = do
  tis' <- mapM (recurM f) tis
  f (meta :< TreeNode tis')

data IdentOrHole
  = Ident Identifier
  | Hole Identifier
  deriving (Show, Eq)

-- weaktype
-- WT ::= P | N
data WeakType
  = WeakTypeVar Identifier
  | WeakTypeHole Identifier
  | WeakTypeNode Identifier
                 [WeakType]
  | WeakTypeUp WeakType
  | WeakTypeDown WeakType
                 Identifier
  | WeakTypeUniv WeakLevel
  | WeakTypeForall (IdentOrHole, WeakType)
                   WeakType
  deriving (Show, Eq)

-- value type
-- P ::= p
--     | (down N)
--     | {defined constant type}
--     | (node (x P) P)
--     | (p p)
--     | (universe i)
data ValueType
  = ValueTypeVar Identifier
  | ValueTypeNode Identifier
                  [ValueType]
  | ValueTypeDown CompType
  | ValueTypeUniv Level
  deriving (Show, Eq)

-- computation type
-- N ::= (forall (x P) N)
--     | (up P)
data CompType
  = CompTypeForall (Identifier, ValueType)
                   CompType
  | CompTypeUp ValueType
  deriving (Show, Eq)

data Type
  = TypeValueType ValueType
  | TypeCompType CompType
  deriving (Show)

weakenValueType :: ValueType -> WeakType
weakenValueType (ValueTypeVar i) = WeakTypeVar i
weakenValueType (ValueTypeNode s ts) = do
  let ts' = map weakenValueType ts
  WeakTypeNode s ts'
weakenValueType (ValueTypeDown c) = WeakTypeDown (weakenCompType c) "ANY"
weakenValueType (ValueTypeUniv l) = WeakTypeUniv (WeakLevelFixed l)

weakenCompType :: CompType -> WeakType
weakenCompType (CompTypeForall (i, t1) t2) = do
  let t1' = weakenValueType t1
  let t2' = weakenCompType t2
  WeakTypeForall (Ident i, t1') t2'
weakenCompType (CompTypeUp v) = WeakTypeUp (weakenValueType v)

data PatF a
  = PatVar Identifier
  | PatApp Identifier
           [a]
  deriving (Show, Eq)

$(deriveShow1 ''PatF)

deriving instance Functor PatF

type Pat = Cofree PatF Meta

type Occurrence = [Int]

data Decision a
  = DecisionLeaf [Identifier]
                 a
  | DecisionFail
  | DecisionSwitch Occurrence
                   [((Identifier, [Identifier]), Decision a)]
  | DecisionSwap Int
                 (Decision a)
  deriving (Show)

deriving instance Functor Decision

$(deriveShow1 ''Decision)

-- value / positive term
-- v ::= x
--     | {defined constant} <- nat, succ, etc.
--     | (v v)
--     | (thunk e)
data ValueF c v
  = ValueVar Identifier
  | ValueNodeApp Identifier
                 [v]
  | ValueThunk c
               Identifier
  deriving (Show)

-- computation / negative term
-- e ::= (lambda (x P) e)
--     | (e v)
--     | (return v)
--     | (bind (x P) e1 e2)
--     | (unthunk v)
--     | (mu (x P) e)
--     | (case e (v1 e1) ... (vn en))
data CompF v c
  = CompLam Identifier
            c
  | CompApp c
            v
  | CompRet v
  | CompBind Identifier
             c
             c
  | CompUnthunk v
                Identifier
  | CompMu Identifier
           c
  | CompCase [v]
             (Decision c)
  deriving (Show)

$(deriveShow1 ''ValueF)

$(deriveShow1 ''CompF)

data VMeta = VMeta
  { vtype :: ValueType
  } deriving (Show)

data CMeta = CMeta
  { ctype :: CompType
  } deriving (Show)

newtype Value =
  Value (Cofree (ValueF Comp) VMeta)
  deriving (Show)

newtype Comp =
  Comp (Cofree (CompF Value) CMeta)
  deriving (Show)

data Term
  = TermValue Value
  | TermComp Comp
  deriving (Show)

data WeakTermF a
  = WeakTermVar Identifier
  | WeakTermNodeApp Identifier
                    [a]
  | WeakTermThunk a
  | WeakTermLam (Identifier, WeakType)
                a
  | WeakTermApp a
                a
  | WeakTermRet a
  | WeakTermBind (Identifier, WeakType)
                 a
                 a
  | WeakTermUnthunk a
  | WeakTermMu (Identifier, WeakType)
               a
  | WeakTermCase [a]
                 [([Pat], a)]
  | WeakTermAsc a
                WeakType

$(deriveShow1 ''WeakTermF)

deriving instance Show a => Show (WeakTermF a)

deriving instance Functor WeakTermF

type WeakTerm = Cofree WeakTermF Meta

instance (Show a) => Show (IORef a) where
  show a = show (unsafePerformIO (readIORef a))

type ValueInfo = (Identifier, [(Identifier, ValueType)], ValueType)

data Env = Env
  { count          :: Int -- to generate fresh symbols
  , valueEnv       :: [ValueInfo] -- defined values
  , constructorEnv :: [(Identifier, IORef [Identifier])]
  , notationEnv    :: [(Tree, Tree)] -- macro transformers
  , reservedEnv    :: [Identifier] -- list of reserved keywords
  , nameEnv        :: [(Identifier, Identifier)] -- used in alpha conversion
  , weakTypeEnv    :: [(Identifier, WeakType)] -- used in type inference
  , typeEnv        :: [(Identifier, Type)] -- polarized type environment
  , constraintEnv  :: [(WeakType, WeakType)] -- used in type inference
  , levelEnv       :: [(WeakLevel, WeakLevel)] -- constraint regarding the level of universes
  , argEnv         :: [(IdentOrHole, IdentOrHole)] -- equivalence of arguments of forall
  , thunkEnv       :: [(Identifier, Identifier)]
  , funEnv         :: [(Identifier, IORef [(Identifier, IORef Code)])]
  , scope          :: Identifier -- used in Virtual to determine the name of current function
  } deriving (Show)

initialEnv :: Env
initialEnv =
  Env
    { count = 0
    , valueEnv = []
    , constructorEnv = []
    , notationEnv = []
    , reservedEnv =
        [ "thunk"
        , "lambda"
        , "return"
        , "bind"
        , "unthunk"
        , "mu"
        , "case"
        , "ascribe"
        , "down"
        , "universe"
        , "forall"
        , "up"
        ]
    , nameEnv = []
    , weakTypeEnv = []
    , typeEnv = []
    , constraintEnv = []
    , levelEnv = []
    , thunkEnv = []
    , argEnv = []
    , funEnv = []
    , scope = "main"
    }

type WithEnv a = StateT Env (ExceptT String IO) a

runWithEnv :: WithEnv a -> Env -> IO (Either String (a, Env))
runWithEnv c env = runExceptT (runStateT c env)

evalWithEnv :: (Show a) => WithEnv a -> Env -> IO ()
evalWithEnv c env = do
  x <- runWithEnv c env
  case x of
    Left err -> putStrLn err
    Right (y, env) -> do
      putStrLn $ Pr.ppShow y
      putStrLn $ Pr.ppShow env

newName :: WithEnv Identifier
newName = do
  env <- get
  let i = count env
  modify (\e -> e {count = i + 1})
  return $ "#" ++ show i

newNameWith :: Identifier -> WithEnv Identifier
newNameWith s = do
  i <- newName
  let s' = s ++ i
  modify (\e -> e {nameEnv = (s, s') : nameEnv e})
  return s'

lookupWTEnv :: String -> WithEnv (Maybe WeakType)
lookupWTEnv s = gets (lookup s . weakTypeEnv)

lookupTEnv :: String -> WithEnv (Maybe Type)
lookupTEnv s = gets (lookup s . typeEnv)

lookupWTEnv' :: String -> WithEnv WeakType
lookupWTEnv' s = do
  mt <- lookupWTEnv s
  case mt of
    Just t -> return t
    Nothing -> do
      env <- get
      lift $
        throwE $
        "the type of " ++
        show s ++
        " is not defined in the type environment. typeEnv:\n" ++
        (Pr.ppShow $ weakTypeEnv env)

lookupVEnv :: String -> WithEnv (Maybe ValueInfo)
lookupVEnv s = do
  env <- get
  return $ find (\(x, _, _) -> x == s) $ valueEnv env

lookupFunEnv :: Identifier -> WithEnv (IORef [(Identifier, IORef Code)])
lookupFunEnv s = do
  m <- gets (lookup s . funEnv)
  case m of
    Nothing -> lift $ throwE $ "no such function: " ++ show s
    Just k  -> return k

lookupThunkEnv :: Identifier -> WithEnv [Identifier]
lookupThunkEnv s = do
  env <- get
  let selector pair =
        case pair of
          (x, y)
            | x == s -> [y]
          (x, y)
            | y == s -> [x]
          _ -> []
  return $ concatMap selector $ thunkEnv env

lookupCodeEnv :: Identifier -> WithEnv (Maybe (IORef Code))
lookupCodeEnv s = do
  current <- getFunName
  codeEnvRef <- lookupFunEnv current
  codeEnv <- liftIO $ readIORef codeEnvRef
  return $ lookup s codeEnv

lookupConstructorEnv :: Identifier -> WithEnv [Identifier]
lookupConstructorEnv cons = do
  env <- get
  case lookup cons (constructorEnv env) of
    Nothing -> lift $ throwE $ "no such constructor defined: " ++ show cons
    Just cenvRef -> liftIO $ readIORef cenvRef

insWTEnv :: String -> WeakType -> WithEnv ()
insWTEnv s t = modify (\e -> e {weakTypeEnv = (s, t) : weakTypeEnv e})

insCEnv :: WeakType -> WeakType -> WithEnv ()
insCEnv t1 t2 = modify (\e -> e {constraintEnv = (t1, t2) : constraintEnv e})

insLEnv :: WeakLevel -> WeakLevel -> WithEnv ()
insLEnv l1 l2 = modify (\e -> e {levelEnv = (l1, l2) : levelEnv e})

insAEnv :: IdentOrHole -> IdentOrHole -> WithEnv ()
insAEnv x y = modify (\e -> e {argEnv = (x, y) : argEnv e})

insConstructorEnv :: Identifier -> Identifier -> WithEnv ()
insConstructorEnv i cons = do
  env <- get
  case lookup i (constructorEnv env) of
    Nothing -> do
      cenvRef <- liftIO $ newIORef [cons]
      modify (\e -> e {constructorEnv = (i, cenvRef) : constructorEnv env})
    Just cenvRef -> do
      cenv <- liftIO $ readIORef cenvRef
      liftIO $ writeIORef cenvRef (cons : cenv)

insThunkEnv :: Identifier -> Identifier -> WithEnv ()
insThunkEnv i j = modify (\e -> e {thunkEnv = (i, j) : thunkEnv e})

insCodeEnv :: Identifier -> IORef Code -> WithEnv ()
insCodeEnv i code = do
  current <- getFunName
  codeEnvRef <- lookupFunEnv current
  codeEnv <- liftIO $ readIORef codeEnvRef
  liftIO $ writeIORef codeEnvRef $ (i, code) : codeEnv

insEmptyFunEnv :: Identifier -> WithEnv ()
insEmptyFunEnv i = do
  x <- liftIO $ newIORef []
  modify (\e -> e {funEnv = (i, x) : funEnv e})

setFunName :: Identifier -> WithEnv ()
setFunName i = do
  modify (\e -> e {scope = i})

getFunName :: WithEnv Identifier
getFunName = do
  env <- get
  return $ scope env

local :: WithEnv a -> WithEnv a
local p = do
  env <- get
  x <- p
  modify (\e -> env {count = count e})
  return x

foldMTerm ::
     (Cofree f Meta -> a -> f (Cofree f Meta))
  -> Cofree f Meta
  -> [a]
  -> StateT Env (ExceptT String IO) (Cofree f Meta)
foldMTerm _ e [] = return e
foldMTerm f e (t:ts) = do
  let tmp = f e t
  i <- newName
  foldMTerm f (Meta {ident = i} :< tmp) ts

data Data
  = DataPointer Identifier -- var is something that points already-allocated data
  | DataCell Identifier -- value of defined data types
             [Data]
  | DataLabel Identifier -- the address of quoted code
  deriving (Show, Eq)

data Code
  = CodeReturn Data -- return
  | CodeLet Identifier -- bind (we also use this to represent application)
            Data
            Code
  | CodeCall Identifier -- the result of call
             Identifier -- the label of the funtion
             [Identifier] -- arguments
             Code -- continuation
  | CodeJump Identifier -- unthunk
             Identifier -- this second argument is required to lookup the corresponding code
             [Identifier] -- list of arguments
  deriving (Show, Eq)
