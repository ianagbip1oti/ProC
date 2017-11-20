module ProC.Parser.Lexer
  ( braces
  , identifier
  , integer
  , parens
  , reserved
  , reservedOp
  , semi
  , stringLiteral
  , symbol
  , whiteSpace
  ) where

import           ProC.Language
import           ProC.Parser.ProC

import           Text.Parsec
import qualified Text.Parsec.Token as Token

procDef :: Token.GenLanguageDef String u IO
procDef =
  Token.LanguageDef
  { Token.caseSensitive = False
  , Token.commentStart = "/*"
  , Token.commentEnd = "*/"
  , Token.commentLine = "//"
  , Token.identStart = letter
  , Token.identLetter = alphaNum
  , Token.nestedComments = True
  , Token.opStart = Token.opLetter procDef
  , Token.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.reservedNames = ["fls", "int", "print", "str", "tru", "whl"]
  , Token.reservedOpNames =
      [ "="
      , "++"
      , "+"
      , "-"
      , "*"
      , "/"
      , "!"
      , "&&"
      , "||"
      , "=="
      , "!="
      , ">="
      , ">"
      , "<="
      , "<"
      ]
  }

braces :: Parser a -> Parser a
braces = Token.braces lexer

lexer :: Token.GenTokenParser String u IO
lexer = Token.makeTokenParser procDef

identifier :: Parser Identifier
identifier = Identifier <$> Token.identifier lexer

integer :: Parser Integer
integer = Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

semi :: Parser String
semi = Token.semi lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
