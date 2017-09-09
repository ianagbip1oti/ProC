module ProC.Parser.Statement
  ( statement
  , statements
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

strVarDeclStatement :: Parser Statement
strVarDeclStatement = do
  reserved "str"
  name <- identifier
  reservedOp "="
  expr <- stringExpression
  return $ StrVarDecl name expr

noopStatement :: Parser Statement
noopStatement = whiteSpace >> return Noop

statement :: Parser Statement
statement =
  printStatement <|> intVarDeclStatement <|> strVarDeclStatement <|>
  noopStatement

statements :: Parser Statement
statements = do
  whiteSpace
    -- TODO: We allow input that doens't finish with a final ;
  list <- semiSep1 statement
  eof
  return $ Seq list
