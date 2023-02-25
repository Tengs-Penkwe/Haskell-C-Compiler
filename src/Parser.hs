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
   -- <|> try function
   <?> "block"

declaration :: Parser Block
declaration = do
  dlist   <- declList <* semi
  return $ Decl dlist
  <?> "declaration"

-- function :: Parser Block
-- function = do
--   (tp, name, params) <- funcDecl
--   stmt <- statement
--   return $ Func tp name params stmt

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
  return $ getDecl tp name

getDecl :: Type -> [(String, DirectDeclarator)] -> DeclList
getDecl t str_decl = foldr f [] str_decl
  where f (ptr, direct) acc = (checkPointer ptr t, direct):acc
        checkPointer ptr t = if ptr == "*" then Pointer t else t 

pointer :: Parser String
pointer = length <$> many (symbol "*")

declar :: Parser (String, InitDeclarator, )
declar = do
  ptr   <- pointer 
  decl  <- (try funcDirect <|> variableDirect)
  -- arr   <- array
  init <- optionMaybe assignExpr
  return (ptr, case init of
                  Just e  -> (decl, e)
                  Nothing -> (decl, VoidExpr) )

-- array :: Parser String

getDecl :: Type -> [(String, InitDeclarator)] -> DeclarList
getDecl tp init_decl = foldr f []   

declarator :: Parser (String, DirectDeclarator)
declarator = do
  ptr  <- pointer
  decl <- (try funcDirect <|> variableDirect)
  return (ptr, decl)

funcDirect :: Parser DirectDeclarator
funcDirect = do 
  name    <- identifier
  params  <- parens paramList 
  return $ Funct name params 

variableDirect :: Parser DirectDeclarator
variableDirect = do
  var <- identifier
  sizes <- many $ char '[' *> integer <* char ']'
  let directDecl = case sizes of
        [] -> Var var 
        _  -> foldr (\s acc -> Array var s ) (Var var ) sizes
  return directDecl
  -- var     <- identifier
  -- size    <- optionMaybe $ char '[' >> integer >>= \n -> char ']' >> return n
  -- return $ case size of
  --   Nothing -> Var var 
  --   Just s -> Array var s 

{-- ========================================
 -                List
 - ======================================== --}
paramList :: Parser DeclList
paramList = do
  params <- commaSep param
  return $ concat params
  where 
    param = do 
      tp    <- specifier
      name  <- declarator
      return $ getDecl tp [name]

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
  <|> try assignStmt
  <?> "statement"

voidStmt :: Parser Stmt
voidStmt = do
  _     <- semi
  return VoidStmt

assignStmt :: Parser Stmt
assignStmt = do
  var     <- expr
  assign  <- expr
  return $ AssignStmt var assign

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
  <|> assignExpr
  <|> variable
  <|> parens expr

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

assignExpr :: Parser Expr
assignExpr = do
  _    <- reservedOp "="
  expr <- expr
  return $ Assign expr

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

