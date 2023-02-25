module Syntax where

type Program = [Block]
type Name = String 
type Size = Integer

data Block
  = Decl      DeclList
  | Func      Type Name ParamList Stmt
  -- | ProtoFunc Type Name ParamList
  deriving(Show)

type Declaration  = (Type, DirectDeclarator)
type DeclList     = [Declaration]
type ParamList    = DeclList


type Declar       = (Type, Name, Expr)
type DeclarList   = [Declar]


-- (*foo)(double)
type InitDeclarator = (String, Name, String, Expr)

data DirectDeclarator 
  = Var       Name 
  | Funct     Name ParamList 
  | Array     Name Integer 
  deriving(Eq, Ord, Show) 

-- data InitDeclarator
--   = 
--   | 

-- data Initializer 
--   = 
--   | 
--   |

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
  | Function ParamList
  | Arr      Type --Size
  | Pointer  Type
  deriving (Eq, Ord, Show)

data ConstVal
  = Floating  Double
  | Integer   Integer
  | Character Char
  | String    String
  deriving (Eq, Ord, Show)

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
  deriving(Show)

data Expr
  = Const    Type ConstVal
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
