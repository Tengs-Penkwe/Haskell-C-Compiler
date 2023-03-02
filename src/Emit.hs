module Emit where

import Codegen
import qualified Syntax as S

import LLVM.Context(createContext)
import LLVM.Module      -- For File, try to delete this import
import LLVM.Module(createModuleFromAST, writeLLVMAssemblyToFile)

import LLVM.AST.Type(void, i8, i16, i32, i64, ptr, float, double)

-- import LLVM.AST
import qualified LLVM.AST as AST

import qualified Data.ByteString.Internal as BS

import Control.Monad.Except


{-- ==================================================
 -      AST.Moudle + Context -> Moudle -> IR Assembly 
 - ================================================== --}
codegen :: AST.Module -> [S.Unit] -> IO String
codegen emptyModule units = do
  context      <- createContext
  llvmModule   <- createModuleFromAST context astModule
  llstr        <- moduleLLVMAssembly llvmModule
  return $ BS.unpackChars llstr
  -- writeLLVMAssemblyToFile (File "output.ll") llvmModule
  where
    llvm = mapM unitToLLVM units
    astModule = runLLVM emptyModule llvm

{-- ==================================================
 -      Syntax.Unit + LLVM -> AST.Module
 - ================================================== --}
unitToLLVM :: S.Unit -> LLVM()
unitToLLVM (S.Declaration decl)
  = declaration decl

{-- ==================================================
 -      Declarations
 - ================================================== --}
declaration :: S.Declaration -> LLVM()
declaration (typ, (directDecl, expr))
  = case directDecl of
    S.Var name -> do
      globalDefine (typeConvert typ) name 
    S.Array name _ -> do
      globalDefine (typeConvert typ) name 

{-- ==================================================
 -      Type Conversion
 - ================================================== --}
typeConvert :: S.Type -> AST.Type
typeConvert S.Char  = i8
typeConvert S.Short = i16
typeConvert S.Int   = i32
typeConvert S.Long  = i64
typeConvert S.Float = float
typeConvert S.Double = double

