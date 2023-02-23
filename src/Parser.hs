module Parser where

import Token
import Syntax

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

{-- ========================================
 -           Program to Blocks
 - ======================================== --}
parseProgram :: String -> Either ParseError Program
parseProgram s = parse getBlocks "<stdin>" s

getBlocks :: Parser Program
getBlocks = do
  blks   <- many1 block
  return blks

{-- ========================================
 -                Blocks
 - ======================================== --}
-- These are the three possible blocks
block :: Parser Block
block 
    =  try declaration
   <|> try function
   <|> try protoFunc
   <?> "block"

declaration :: Parser Block
declaration = do
  dlist   <- declList <* semi
  return $ Decl dlist
  <?> "declaration"

protoFunc :: Parser Block
protoFunc = do
  (tp, name, params) <- funcDecl
  return $ ProtoFunc tp name params

function :: Parser Block
function = do
  (tp, name, params) <- funcDecl
  stmt <- statement
  return $ Func tp name params stmt

{-- ========================================
 -                Specifier
 - ======================================== --}
specifier :: Parser Type
specifier 
   =  (reserved "void"  >> return Void)
  <|> (reserved "char"  >> return Char)
  <|> (reserved "short" >> return Short)
  <|> (reserved "int"   >> return Int)
  <|> (reserved "long"  >> return Long)
  <|> (reserved "float" >> return Float)
  <|> (reserved "double"  >> return Double)
  <?> "valid type"

{-- ========================================
 -                Declaration
 - ======================================== --}
declList :: Parser DeclList
declList = do 
  tp    <- specifier
  name  <- commaSep declarator 
  return $ genDecl tp name

genDecl :: Type -> [(String, DirectDeclarator)] -> DeclList
genDecl t str_decl = foldr f [] str_decl
  where f (ptr, direct) acc = (checkPointer ptr t, direct):acc
        checkPointer ptr t = if ptr == "*" then Pointer t else t 

declarator :: Parser (String, DirectDeclarator)
declarator = do
  ptr  <- pointer
  decl <- directDecl
  return $ (ptr, decl)
  where pointer = option "" $ (reserved "*" >> return "*")

funcDecl :: Parser (Type, Name, ParamList)
funcDecl = do
  tp     <- specifier
  name   <- identifier
  params <- parens $ commaSep paramDecl
  return (tp, name, params)

paramDecl :: Parser Param
paramDecl = do
  tp    <- specifier
  name  <- identifier
  return (tp, name)

directDecl :: Parser DirectDeclarator
directDecl
   =  try arrayDecl
  <|> try variableDecl

arrayDecl :: Parser DirectDeclarator
arrayDecl 
   =  try (do var     <- identifier 
              length  <- brackets integer
              _       <- symbol "="
              stmt    <- statement
              return $ Array var length stmt)
  <|> try (do var     <- identifier 
              length  <- brackets integer
              return $ Array var length VoidStmt)

variableDecl :: Parser DirectDeclarator
variableDecl
   =  try (do var     <- identifier
              return $ Var var VoidStmt)
  <|> try (do var     <- identifier
              _       <- symbol "="
              stmt    <- statement
              return $ Var var stmt)


{-- ========================================
 -                Statement 
 - ======================================== --}
statement :: Parser Stmt
statement
   =  try exprStmt 
  <|> try voidStmt
  <|> try compoundStmt
  <|> try ifStmt
  <|> try whileStmt
  <|> try retStmt
  <?> "statement"

voidStmt :: Parser Stmt
voidStmt = do
  _     <- semi
  return VoidStmt

exprStmt :: Parser Stmt
exprStmt = do
  expression <- expr <* semi
  return $ ExprStmt expression

compoundStmt :: Parser Stmt
compoundStmt = do
  _         <- symbol "{"
  declList  <- many $ try (declList <* semi)
  stmtList  <- many $ try statement 
  _         <- symbol "}"
  return $ CompoundStmt (concat declList) stmtList
  <?> "compoundStmt"

ifStmt :: Parser Stmt
ifStmt 
  =   try (do reserved "if"
              cond    <- parens expr
              ifExec  <- braces statement
              reserved "else" 
              elseExec<- braces statement
              return $ IfStmt cond ifExec elseExec)
  <|> try (do reserved "if"
              cond    <- parens expr
              ifExec  <- statement
              return $ IfStmt cond ifExec VoidStmt)

whileStmt :: Parser Stmt
whileStmt
  =   try (do reserved "while"
              cond  <- parens expr
              execs <- braces statement
              return $ IterStmt cond execs)

retStmt :: Parser Stmt
retStmt 
  =   try (do reserved "return"
              exec <- statement
              return $ RetStmt exec)
  <|> try (do reserved "return"
              return $ RetStmt VoidStmt)

{-- ========================================
 -                Expression
 - ======================================== --}

expr :: Parser Expr
expr = Ex.buildExpressionParser binOpTable factor

factor :: Parser Expr
factor
   =  try constant
  <|> try call
  <|> variable
  <|> parens expr

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

{-- Binary Operator Function --}
-- helper function
binary symbol func associativity = Ex.Infix (reservedOp symbol >> return (BinaryOp func)) associativity

binOpTable = [[binary "*"  Multiple Ex.AssocLeft, binary "/"  Divide Ex.AssocLeft]
             ,[binary "+"  Plus     Ex.AssocLeft, binary "-"  Minus  Ex.AssocLeft]
             ,[binary "==" Equal    Ex.AssocLeft, binary "!=" NotEq  Ex.AssocLeft]
             ,[binary "<"  Less     Ex.AssocLeft, binary ">"  More   Ex.AssocLeft,
               binary "<=" LessEq   Ex.AssocLeft, binary ">=" MoreEq Ex.AssocLeft]
             ,[binary "&&" And      Ex.AssocLeft, binary "||" Or     Ex.AssocLeft]]

{-- Parse Constant in Expr
 - can be float or int --}
constant :: Parser Expr
constant 
  =   try (do
            n <- float
            return $ Const Float (Floating n))
  <|> try (do
            n <- integer
            return $ Const Int (Integer n))
  <?> "Not a legal Constant"

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Variable var

