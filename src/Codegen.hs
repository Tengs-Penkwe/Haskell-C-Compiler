{-# LANGUAGE DerivingVia #-}
module gen where

import Parser
import Syntax

-- import Data.Functor.Identity

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST

import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP

-- newtype LLVM a = LLVM (State AST.Module a)
--   deriving (Functor, Applicative, Monad, MonadState AST.Module )

-- runLLVM :: AST.Module -> LLVM a -> AST.Module
-- runLLVM mod (LLVM m) = execState m mod


type Code = String
-- newtype genM a
--   = genM (Identity a)
--   deriving (Functor, Applicative, Monad)
--   via Identity

-- rungen :: genM a -> 

genProgram :: Program -> Code
genProgram [] = ""
genProgram (block:blocks) = "hah"--genBlock block ++ genProgram blocks

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
-- -- genDirect (Array name size _) = name ++ "[" ++ show size ++ "]"

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

