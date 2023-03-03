module Syntax where

type Program = [Unit]
type Name = String 
type Size = Integer

data Unit
  = Declaration    Declaration
  | Function       Type Name ParamList Stmt
  deriving(Show)


type Declaration  = (Type, InitDeclarator)
type DeclList     = [Declaration]
-- type Param        = (Type, Name) -- To be deleted
type ParamList    = DeclList

-- (*foo)(double)
type InitDeclarator = (DirectDeclarator, Expr)

data DirectDeclarator 
  = Var       Name 
  -- | Funct     Name ParamList 
  | Array     Name  Integer -- Expr
  deriving(Eq, Ord, Show) 

{-- ========================================
 -                Types
 - ======================================== --}
data Type
  = Void
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  -- | Function ParamList
  | Arr      Expr Type --Size
  | Pointer  Type
  deriving (Eq, Ord, Show)

data ConstVal
  = Floating  Double
  | Integer   Integer
  | Character Char
  | String    String
  deriving (Eq, Ord, Show)

data DeclSpecifier
  = Type Type
  | Store StoreSpecifier
  -- | type-qualifier

data StoreSpecifier 
  = Auto
  | Register
  | Static
  | Extern
  | Typedef

data TypeQualifier
  = Const
  | Volatile

{-- ========================================
 -           Statement & Expression
 - ======================================== --}
data Stmt
  = ExprStmt      Expr         -- An experession      
  | CompoundStmt  DeclList [Stmt]
  | IfStmt        Expr Stmt Stmt
  | IterStmt      Expr Stmt      --While
  | RetStmt       Stmt
  | AssignStmt    Expr Expr
  | VoidStmt              -- Does nothing 
  deriving(Eq, Ord, Show)

data Expr
  = Constant    Type ConstVal
  | BinaryOp BinOp Expr Expr
  -- | UnaryOp  UnOp
  | Assign   Expr
  | Variable Name
  | Call     Name [Expr]
  | VoidExpr
  deriving (Eq, Ord, Show)

{-- ========================================
 -                Operators
 - ======================================== --}
data UnOp
  = Addr
  | Deref
  | Pos
  | Neg
  | Not
  | LogicNot

data AssignOp
  = Eq
  | MultipleEq
  | DivideEq
  | ModEq
  | PlusEq
  | MinusEq
  | LeftShiftEq
  | RightShiftEq
  | AndEq
  | XorEq
  | OrEq

data BinOp 
  = Plus
  | Minus
  | Multiple
  | Divide
  | And
  | Or
  | More
  | MoreEq
  | Less
  | LessEq
  | Equal
  | NotEq
  deriving (Eq, Ord, Show)
