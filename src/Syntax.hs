module Syntax where

{-- Everything inside C is a statement --}
data Stmt
  = ExprStmt Expr         -- An experession      
  | VoidStmt              -- Does nothing 
  -- | CompoundStmt Dela
  | IfStmt   Expr Stmt Stmt
  | IterStmt Expr Stmt      --While
  | RetStmt  Stmt
  deriving(Show)

type Name = String 

data Expr
  = Const Type ConstVal
  | BinaryOp Op Expr Expr
  | Var String
  | Call Name [Expr]
  | Function Name [Expr] Expr
  deriving (Eq, Ord, Show)

{-- Literal number 
 3      => Integer 3
 -4.12  => Float  -4.12
 --}
data ConstVal
  = Floating Double
  | Integer Integer
  deriving (Eq, Ord, Show)

data Type
  = Void
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  deriving (Eq, Ord, Show)

data Op 
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
