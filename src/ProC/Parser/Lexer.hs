module ProC.Parser.Lexer
  ( braces
  , identifier
  , integer
  , parens
  , reserved
  , reservedOp
  , semi
  , semiSep1
  , stringLiteral
  , symbol
  , whiteSpace
  ) where

import           ProC.Language
import           ProC.Parser.ProC

import           Data.Functor.Identity

import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token     as Token

procDef :: LanguageDef a
procDef =
  emptyDef
  { Token.commentStart = "/*"
  , Token.commentEnd = "*/"
  , Token.commentLine = "//"
  , Token.identStart = letter
  , Token.identLetter = alphaNum
  , Token.reservedNames = ["fls", "int", "print", "str", "tru"]
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

lexer :: Token.GenTokenParser String u Identity
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

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Token.semiSep1 lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
