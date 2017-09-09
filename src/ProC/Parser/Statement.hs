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

varDeclStatement :: String -> Parser e -> (Identifier -> e -> Statement) -> Parser Statement
varDeclStatement res exprP decl = do
  reserved res
  name <- identifier
  defined <- isDefinedM name
  when defined . fail $ "Already defined: " ++ show name
  reservedOp "="
  expr <- exprP
  insertVariableM name
  return $ decl name exprgit 

intVarDeclStatement :: Parser Statement
intVarDeclStatement = varDeclStatement "int" numericExpression IntVarDecl
  
strVarDeclStatement :: Parser Statement
strVarDeclStatement = varDeclStatement "str" stringExpression StrVarDecl
  
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
