module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Token
import Syntax

getStatements :: String -> Either ParseError [Stmt]
getStatements s = parse (contents splitLine) "<stdin>" s

{-- --}
contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

splitLine :: Parser [Stmt]
splitLine = many $ do
  def <- statement
  reservedOp ";"
  return def

{-- ====================
 -  Expression
 -  every expression in C can be seem as an expression-statement
 - ==================== --}

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
  return $ Var var

{-- ====================
 -  Statement 
 - ==================== --}
statement :: Parser Stmt
statement = try ifStmt
  <|> try whileStmt
  <|> try retStmt
  <|> exprStmt
  <?> "statement"

exprStmt :: Parser Stmt
exprStmt = do
  expression <- expr
  return $ ExprStmt expression

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

