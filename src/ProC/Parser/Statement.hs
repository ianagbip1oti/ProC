module ProC.Parser.Statement where

import ProC.Language
import ProC.Parser.Lexer
import ProC.Parser.NumericExpression
import ProC.Parser.StringExpression

import Text.Parsec
import Text.Parsec.String

printStatement :: Parser Statement
printStatement = p stringExpression
  where
    p expr = reserved "print" >> parens expr >>= return . Print
  
intVarDeclStatement :: Parser Statement
intVarDeclStatement = do
  reserved "int"
  name <- identifier
  reservedOp "="
  expr <- numericExpression
  return $ IntVarDecl name expr
 
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
        statement' = printStatement <|> intVarDeclStatement <|> noopStatement