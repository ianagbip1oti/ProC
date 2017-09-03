module ProC.Parser.Statement where

import ProC.Language
import ProC.Parser.Lexer
import ProC.Parser.StringExpression

import Text.Parsec
import Text.Parsec.String

printStatement :: Parser Statement
printStatement = reserved "print" >> arg >>= return . Print
  where
    arg = parens $ stringExpression
  
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