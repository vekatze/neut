module Kernel.Emit.Move.Emit
  ( Handle,
    new,
    emit,
  )
where

import Data.ByteString.Builder
import Data.ByteString.Builder qualified as L
import Data.ByteString.Lazy qualified as L
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Kernel.Emit.Move.Internal.LowComp qualified as EmitLowComp
import Kernel.Move.Context.Platform qualified as Platform
import Kernel.Move.Scene.Init.Base qualified as Base
import Kernel.Rule.Builder
import Kernel.Rule.Const
import Language.Common.Move.CreateSymbol qualified as Gensym
import Language.Common.Rule.BaseLowType qualified as BLT
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.ForeignCodType qualified as FCT
import Language.Common.Rule.Ident.Reify
import Language.Common.Rule.LowType qualified as LT
import Language.Common.Rule.LowType.EmitLowType
import Language.Common.Rule.LowType.FromBaseLowType qualified as LT
import Language.Common.Rule.PrimNumSize
import Language.Common.Rule.PrimType qualified as PT
import Language.Common.Rule.PrimType.EmitPrimType (emitPrimType)
import Language.LowComp.Move.Reduce qualified as Reduce
import Language.LowComp.Rule.DeclarationName qualified as DN
import Language.LowComp.Rule.LowComp qualified as LC
import Language.LowComp.Rule.LowComp.EmitValue

newtype Handle = Handle
  { baseHandle :: Base.Handle
  }

new :: Base.Handle -> Handle
new baseHandle = do
  Handle {..}

emit :: Handle -> LC.LowCode -> IO L.ByteString
emit h lowCode = do
  case lowCode of
    LC.LowCodeMain mainDef lowCodeInfo -> do
      main <- emitMain h mainDef
      let argDef = emitArgDef
      (header, body) <- emitLowCodeInfo h lowCodeInfo
      return $ buildByteString $ header ++ argDef ++ main ++ body
    LC.LowCodeNormal lowCodeInfo -> do
      let argDecl = emitArgDecl
      (header, body) <- emitLowCodeInfo h lowCodeInfo
      return $ buildByteString $ header ++ argDecl ++ body

emitLowCodeInfo :: Handle -> LC.LowCodeInfo -> IO ([Builder], [Builder])
emitLowCodeInfo h (declEnv, defList, staticTextList) = do
  let declStrList = emitDeclarations declEnv
  let baseSize = Platform.getDataSizeValue (Base.platformHandle (baseHandle h))
  let staticTextList' = map (emitStaticText baseSize) staticTextList
  defStrList <- concat <$> mapM (emitDefinitions h) defList
  return (declStrList <> staticTextList', defStrList)

emitArgDecl :: [Builder]
emitArgDecl = do
  let argc = emitGlobalExt unsafeArgcName LT.Pointer
  let argv = emitGlobalExt unsafeArgvName LT.Pointer
  [argc, argv]

emitArgDef :: [Builder]
emitArgDef = do
  let argc = emitGlobal unsafeArgcName LT.Pointer LC.Null
  let argv = emitGlobal unsafeArgvName LT.Pointer LC.Null
  [argc, argv]

buildByteString :: [Builder] -> L.ByteString
buildByteString =
  L.toLazyByteString . unlinesL

emitGlobal :: T.Text -> LT.LowType -> LC.Value -> Builder
emitGlobal name lt v =
  "@"
    <> TE.encodeUtf8Builder name
    <> " = global "
    <> emitLowType lt
    <> " "
    <> emitValue v

emitGlobalExt :: T.Text -> LT.LowType -> Builder
emitGlobalExt name lt =
  "@"
    <> TE.encodeUtf8Builder name
    <> " = external global "
    <> emitLowType lt

type StaticTextInfo = (T.Text, (Builder, Int))

emitStaticText :: Int -> StaticTextInfo -> Builder
emitStaticText baseSize (from, (text, len)) = do
  "@"
    <> TE.encodeUtf8Builder ("\"" <> from <> "\"")
    <> " = private unnamed_addr constant "
    <> emitLowType (LT.textType baseSize len)
    <> " {"
    <> "i"
    <> intDec baseSize
    <> " 0, "
    <> "i"
    <> intDec baseSize
    <> " "
    <> intDec len
    <> ", "
    <> emitLowType (LT.textTypeInner len)
    <> " c\""
    <> text
    <> "\""
    <> "}"

emitDeclarations :: DN.DeclEnv -> [Builder]
emitDeclarations declEnv = do
  map declToBuilder $ List.sort $ HashMap.toList declEnv

emitDefinitions :: Handle -> LC.Def -> IO [Builder]
emitDefinitions h (name, (args, body)) = do
  args' <- mapM (Gensym.newIdentFromIdent (Base.gensymHandle (baseHandle h))) args
  let sub = IntMap.fromList $ zipWith (\from to -> (toInt from, LC.VarLocal to)) args args'
  let reduceHandle = Reduce.new (Base.gensymHandle (baseHandle h))
  body' <- Reduce.reduce reduceHandle sub body
  let args'' = map (emitValue . LC.VarLocal) args'
  emitDefinition h "ptr" (DD.toBuilder name) args'' body'

emitMain :: Handle -> LC.DefContent -> IO [Builder]
emitMain h (args, body) = do
  let baseSize = Platform.getDataSizeValue (Base.platformHandle (baseHandle h))
  let mainType = emitPrimType $ PT.Int (IntSize baseSize)
  let args' = map (emitValue . LC.VarLocal) args
  emitDefinition h mainType "main" args' body

declToBuilder :: (DN.DeclarationName, ([BLT.BaseLowType], FCT.ForeignCodType BLT.BaseLowType)) -> Builder
declToBuilder (name, (dom, cod)) = do
  let name' = DN.toBuilder name
  "declare "
    <> emitLowType (FCT.fromForeignCodType cod)
    <> " @"
    <> name'
    <> "("
    <> unwordsC (map (emitLowType . LT.fromBaseLowType) dom)
    <> ")"

emitDefinition :: Handle -> Builder -> Builder -> [Builder] -> LC.Comp -> IO [Builder]
emitDefinition h retType name args asm = do
  let header = sig retType name args <> " {"
  emitLowCompHandle <- EmitLowComp.new (baseHandle h) retType
  content <- EmitLowComp.emitLowComp emitLowCompHandle asm
  let footer = "}"
  return $ [header] <> content <> [footer]

sig :: Builder -> Builder -> [Builder] -> Builder
sig retType name args =
  "define fastcc " <> retType <> " @" <> name <> showFuncArgs args
