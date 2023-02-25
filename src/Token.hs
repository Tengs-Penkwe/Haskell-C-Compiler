module Token where

import Text.Parsec

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

{-- Most Important Part --}
-- Our Configuration
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser cStyle

cStyle = emptyDef {
        Tok.commentStart  = "/*"  
      , Tok.commentEnd    = "*/"
      , Tok.commentLine   = "//"
      , Tok.nestedComments= False
      , Tok.caseSensitive = True
      , Tok.reservedOpNames = reservedOperations
      , Tok.reservedNames = reservedNames
}

reservedOperations = [
  "+", "-", "*", "/",      -- Binary Operator
  "&&","||",               -- Logic Operator
  "&", "~", "!",           -- Unary Operator
  ">=","<=",">",  "<",  "==", "!=",        -- Comparator
  "=", "*=","/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|=" ]      --Assignment

reservedNames = [ 
  "void", "char", "short", "int", "long", "float", "double", 
  "if", "else", "while", "do", "switch", "case",
  "return",
  "extern"]

{-- ===============================================
 -   Function to read text and turn it into tokens
 -  =============================================== --}

{-- Integer Token
1, 0, 42, -1, -2 ,-123 
--}
integer :: Parser Integer
integer = Tok.integer lexer

{-- Float Token
1.0, 1.1, 0.12, -123.324, 6674.9
--}
float :: Parser Double
float = Tok.float lexer

{-- Name of Things
x, y, sumOfNumber
--}
identifier :: Parser String
identifier = do 
  _       <- optionMaybe $ symbol "("
  ident   <- Tok.identifier lexer
  _       <- optionMaybe $ symbol ")"
  return ident

{-- Special Symbol --}
symbol :: String -> Parser String
symbol = Tok.symbol lexer

{-- Separator --}
-- separate tokens
whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer
--
semi :: Parser String
semi = Tok.semi lexer
-- separate statements
semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer
-- separate list elements
commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

{-- Three Kinds of Brackets --}
-- ()
parens :: Parser a -> Parser a
parens = Tok.parens lexer
-- {}
braces :: Parser a -> Parser a
braces = Tok.braces lexer
-- []
brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

{-- Reserved Names and Operations --}
reserved :: String -> Parser()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser()
reservedOp = Tok.reservedOp lexer
