{-# LANGUAGE DataKinds #-}
module ProC.Parser where

import ProC.Language

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

procDef = emptyDef
    { Token.commentStart  = "/*"
    , Token.commentEnd    = "*/"
    , Token.commentLine   = "//"
    , Token.identStart    = letter
    , Token.identLetter   = alphaNum
    , Token.reservedNames = ["print"]
    }

lexer = Token.makeTokenParser procDef

parens        = Token.parens        lexer
reserved      = Token.reserved      lexer
semi          = Token.semi          lexer
semiSep1      = Token.semiSep1      lexer
stringLiteral = Token.stringLiteral lexer
symbol        = Token.symbol        lexer
whiteSpace    = Token.whiteSpace    lexer

printStatement :: Parser Statement
printStatement = reserved "print" >> arg >>= return . Print
  where
    arg = parens $ StringLiteral <$> stringLiteral
  
noopStatement :: Parser Statement
noopStatement = whiteSpace >> return Noop

statement :: Parser Statement
statement = do
    whiteSpace
    -- TODO: We allow input that doens't finish with a final ;
    list <- semiSep1 statement'
    eof
    return $ Seq list
    where
        statement' = printStatement <|> noopStatement

parseProC :: String -> Either ParseError Statement
parseProC = parse statement "(Unknown)"
