module ProC.Parser.Lexer where

import Data.Functor.Identity

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

procDef :: LanguageDef a
procDef = emptyDef
    { Token.commentStart    = "/*"
    , Token.commentEnd      = "*/"
    , Token.commentLine     = "//"
    , Token.identStart      = letter
    , Token.identLetter     = alphaNum
    , Token.reservedNames   = ["print"]
    , Token.reservedOpNames = ["++"]
    }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser procDef

integer       :: Parser Integer
integer        = Token.integer       lexer

parens        :: Parser a -> Parser a
parens         = Token.parens        lexer

reserved      :: String -> Parser ()
reserved       = Token.reserved      lexer

reservedOp    :: String -> Parser ()
reservedOp     = Token.reservedOp    lexer

semi          :: Parser String
semi           = Token.semi          lexer

semiSep1      :: Parser a -> Parser [a]
semiSep1       = Token.semiSep1      lexer

stringLiteral :: Parser String
stringLiteral  = Token.stringLiteral lexer

symbol       :: String -> Parser String
symbol        = Token.symbol        lexer

whiteSpace  :: Parser ()
whiteSpace    = Token.whiteSpace    lexer
