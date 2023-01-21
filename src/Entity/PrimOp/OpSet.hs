module Entity.PrimOp.OpSet where

import Data.Set qualified as S
import Data.Text qualified as T

unaryOpSet :: S.Set T.Text
unaryOpSet =
  S.fromList ["fneg"]

convOpSet :: S.Set T.Text
convOpSet =
  S.fromList ["trunc", "zext", "sext", "fptrunc", "fpext", "fptoui", "fptosi", "uitofp", "sitofp"]

cmpOpSet :: S.Set T.Text
cmpOpSet = do
  let s1 = S.map ("icmp " <>) intCmpOpSet
  let s2 = S.map ("fcmp " <>) floatCmpOpSet
  S.union s1 s2

binaryOpSet :: S.Set T.Text
binaryOpSet =
  S.union intBinaryOpSet floatBinaryOpSet

intCmpOpSet :: S.Set T.Text
intCmpOpSet =
  S.fromList ["eq", "ne", "ugt", "uge", "ult", "ule", "sgt", "sge", "slt", "sle"]

floatCmpOpSet :: S.Set T.Text
floatCmpOpSet =
  S.fromList ["false", "oeq", "ogt", "oge", "olt", "ole", "one", "ord", "ueq", "ugt", "uge", "ult", "ule", "une", "uno", "true"]

intBinaryOpSet :: S.Set T.Text
intBinaryOpSet =
  S.fromList ["add", "sub", "mul", "udiv", "sdiv", "urem", "srem", "shl", "lshr", "ashr", "and", "or", "xor"]

floatBinaryOpSet :: S.Set T.Text
floatBinaryOpSet =
  S.fromList ["fadd", "fsub", "fmul", "fdiv", "frem"]
