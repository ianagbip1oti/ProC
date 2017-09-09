module ProC.Parser.Statement
  ( statement
  ) where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.NumericExpression
import           ProC.Parser.ProC
import           ProC.Parser.StringExpression

import           Control.Monad

import           Text.Parsec

printStatement :: Parser Statement
printStatement = p stringExpression
  where
    p expr = Print <$> (reserved "print" >> parens expr)

intVarDeclStatement :: Parser Statement
intVarDeclStatement = do
  reserved "int"
  name <- identifier
  defined <- isDefinedM name
  when defined . fail $ "Already defined: " ++ show name
  reservedOp "="
  expr <- numericExpression
  insertVariableM name
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
