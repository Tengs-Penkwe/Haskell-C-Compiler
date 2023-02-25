module Syntax where

type Program = [Block]
type Name = String 

data Block
  = Decl      DeclList
  | Func      Type Name ParamList Stmt
  | ProtoFunc Type Name ParamList
  deriving(Show)

type Declaration  = (Type, DirectDeclarator)
type DeclList     = [Declaration]
type ParamList    = DeclList


type Param = (Type, Name)  -- To be deleted

data DirectDeclarator 
  = Var       Name 
  | Array     Name Integer 
  | Funct     Name ParamList 
  deriving(Show) 

-- data InitDeclarator
--   = 
--   | 

-- data Initializer 
--   = 
--   | 
--   |

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
  -- | UnaryOp  UnOp
  | Assign   Expr Expr
  | Variable Name
  | Call     Name [Expr]
  deriving (Eq, Ord, Show)


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
  -- | Array
  | Function 
  | Pointer Type
  deriving (Eq, Ord, Show)

{-- ========================================
 -                Operator
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
