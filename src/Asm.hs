module Asm
  ( asmCodeEnv
  , asmCode
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.IORef

import           Data

import           Control.Comonad.Cofree

import qualified Text.Show.Pretty           as Pr

import           Debug.Trace

asmCodeEnv :: WithEnv ()
asmCodeEnv = do
  env <- get
  forM_ (codeEnv env) $ \(name, (args, codeRef)) -> do
    code <- liftIO $ readIORef codeRef
    argRegList <- getArgRegList
    asm <- asmCode code
    asm' <- bindArgs (zip args argRegList) asm
    insAsmEnv name asm'

asmCode :: Code -> WithEnv Asm
asmCode (CodeReturn d) = do
  rax <- getRAX
  tmp <- addMeta $ AsmReturn rax
  asmData rax d tmp
asmCode (CodeLet i d cont) = do
  cont' <- asmCode cont
  asmData i d cont'
asmCode (CodeCall x fun args cont) = do
  cont' <- asmCode cont
  if length args > 6
    then lift $ throwE "Asm.asmCode: the number of arguments exceeds 6"
    else do
      argRegList <- getArgRegList
      rax <- getRAX
      cont'' <- addMeta $ AsmMov x (AsmArgReg rax) cont'
      call <- addMeta $ AsmCall rax fun args cont''
      bindArgs (zip argRegList args) call
asmCode (CodeExtractValue x base i cont) = do
  t <- lookupPolTypeEnv' base
  case t of
    Pos (_ :< PosSigma args _) -> do
      let ts = map snd args
      is <- mapM sizeOfType ts
      let offset = sum $ take i is
      cont' <- asmCode cont
      addMeta $ AsmLoadWithOffset x base offset cont'
    _ -> lift $ throwE "Asm.asmCode : typeError"

bindArgs :: [(Identifier, Identifier)] -> Asm -> WithEnv Asm
bindArgs [] asm = return asm
bindArgs ((to, from):rest) asm = do
  asm' <- bindArgs rest asm
  addMeta $ AsmMov to (AsmArgReg from) asm'

asmData :: Identifier -> Data -> Asm -> WithEnv Asm
asmData reg (DataLocal x) cont = addMeta $ AsmMov reg (AsmArgReg x) cont
asmData reg (DataLabel x) cont = addMeta $ AsmMov reg (AsmArgReg x) cont
asmData reg (DataInt32 i) cont = addMeta $ AsmMov reg (AsmArgImmediate i) cont
asmData reg (DataStruct xs) cont = do
  is <- mapM sizeOf xs
  let size = sum is
  tmp <- setContent reg is cont
  rdi <- getRDI
  call <- addMeta $ AsmCall reg "_malloc" [rdi] tmp
  addMeta $ AsmMov rdi (AsmArgImmediate size) call

setContent :: Identifier -> [Int] -> Asm -> WithEnv Asm
setContent basePointer sizeList cont = do
  undefined

sizeOf :: Identifier -> WithEnv Int
sizeOf x = do
  Pos t <- lookupPolTypeEnv' x
  sizeOfType t

-- byte size of type
sizeOfType :: PrePos -> WithEnv Int
sizeOfType (_ :< PosVar _) =
  lift $ throwE "Asm.sizeOfType: the type of a type variable is not defined"
sizeOfType (_ :< PosPi _ _) =
  lift $ throwE "Asm.sizeOfType: the type of function itself is not defined"
sizeOfType (_ :< PosSigma xts t) = do
  is <- mapM (sizeOfType . snd) xts
  i <- sizeOfType t
  return $ i + sum is
sizeOfType (_ :< PosTop) = return 4
sizeOfType (_ :< PosDown _) = return 4
sizeOfType (_ :< PosUp t) = sizeOfType t
sizeOfType (_ :< PosUniv) = return 4
sizeOfType v = lift $ throwE $ "Asm.sizeOfType: " ++ show v ++ " is not a type"
