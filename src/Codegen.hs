-- {-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codegen where

import LLVM.AST
import LLVM.AST.Type(void, i8, i16, i32, i64, ptr, float, double)
import qualified LLVM.AST as AST

import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad.State

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM modul (LLVM m) = execState m modul

-- execState :: [(String, Oprand)] -> Codegen a ->

{-- ==================================================
 -      States in Code Generation Process
 - ================================================== --}
type SymbolTable = [(String, Operand)]

data CodegenState 
  = CodegenState {
    currentUnit :: Name
  , units       :: Map Name UnitState
  , symtab      :: SymbolTable
  , unitCount   :: Int                      -- Count of units
  , count       :: Int                      -- Count of unamed instrunctions
  , names       :: Names
  }

data UnitState 
  = UnitState {
    index :: Int
  , stack :: [Named Instruction]
  , term  :: Maybe (Named Terminator)
  } deriving Show


{-- ==================================================
 -      Names 
 - ================================================== --}
type Names = Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName name names = 
  case Map.lookup name names of 
    Nothing   -> (name, Map.insert name 1 names)
    Just idx  -> (name ++ show idx, Map.insert name (idx+1) names)



-------------------------------------------------------
-- Old Code 
-------------------------------------------------------

-- newtype genM a
--   = genM (Identity a)
--   deriving (Functor, Applicative, Monad)
--   via Identity

-- genProgram :: Program -> String
-- genProgram [] = ""
-- genProgram (block:blocks) = genBlock block ++ genProgram blocks

-- genBlock :: Block -> Code
-- genBlock (Decl declList) = genDeclList declList ++ "\n"
-- genBlock (ProtoFunc returnType name paramList) = "ProtoFunc" ++ ";\n"
-- genBlock (Func returnType name paramList stmt) = "Fun" ++ "\n"

-- genDeclList ::  [(Type, (DirectDeclarator, Expr))] -> Code
-- genDeclList [] = ""
-- genDeclList ((varType, direct):xs) = genDirect direct ++ genType varType ++ " " ++ genDeclList' xs
--   where genDeclList' [] = ""
--         genDeclList' (_:ys) = "\n" ++genDeclList ys

-- genDirect :: (DirectDeclarator, Expr) -> String
-- genDirect (Var name, _ ) = "@." ++ name ++ " = " ++ " global " 
-- genDirect (Array name size _) = name ++ "[" ++ show size ++ "]"

-- genType :: Type -> Code
-- genType Void    = ""
-- genType Char    = "i8"
-- genType Short   = "i16"
-- genType Int     = "i32"
-- genType Long    = "i64"
-- genType Float   = "float"
-- genType Double  = "double"
-- genType (Pointer t) = "ptr"

-- genExpr :: Expr -> String
-- genExpr (Const t val) = genType t ++ " " ++ genConstVal val
-- genExpr (BinaryOp op left right) = genExpr left ++ " " ++ genBinOp op ++ " " ++ genExpr right
-- genExpr (UnaryOp op) = genUnOp op
-- genExpr (Assign left right) = genExpr left ++ " = " ++ genExpr right
-- genExpr (Variable name) = name
-- genExpr (Call name args) = name ++ "(" ++ genExprList args ++ ")"
-- genExpr (Function name args expr) = genType (Void) ++ " " ++ name ++ "(" ++ genParamList args ++ ")\n{\n" ++ genStmt expr ++ "}\n"

