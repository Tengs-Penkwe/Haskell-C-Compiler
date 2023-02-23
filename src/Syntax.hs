module Syntax where

type Program = [Block]
type Name = String 

data Block
  = Decl      DeclList
  | Func      Type Name ParamList Stmt
  | ProtoFunc Type Name ParamList
  deriving(Show)

data Stmt
  = ExprStmt Expr         -- An experession      
  | VoidStmt              -- Does nothing 
  | CompoundStmt DeclList [Stmt]
  | IfStmt   Expr Stmt Stmt
  | IterStmt Expr Stmt      --While
  | RetStmt  Stmt
  deriving(Show)

data Expr
  = Const    Type ConstVal
  | BinaryOp BinOp Expr Expr
  | UnaryOp  UnOp
  | Assign   Expr Expr
  | Variable Name
  | Call     Name [Expr]
  | Function Name [Expr] Expr
  deriving (Eq, Ord, Show)

type Param        = (Type, Name)
type ParamList    = [Param]
type DeclList     = [(Type, DirectDeclarator)]


data DirectDeclarator 
  = Var       Name Stmt
  | Array     Name Integer Stmt
  deriving(Show)

-- data InitDeclarator
--   = 
--   | 

-- data Initializer 
--   = 
--   | 
--   |

{-- Literal number 
 3      => Integer 3
 -4.12  => Float  -4.12
 ================== --}
data ConstVal
  = Floating  Double
  | Integer   Integer
  | Character Char
  | String    String
  deriving (Eq, Ord, Show)

data Type
  = Void
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  | Pointer Type
  deriving (Eq, Ord, Show)

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
