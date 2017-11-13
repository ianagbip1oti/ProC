module ProC.Parser.Statement
  ( statement
  , statements
  ) where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.PBlnExpression
import           ProC.Parser.PIntExpression
import           ProC.Parser.ProC
import           ProC.Parser.PStrExpression

import           Control.Monad

import           Text.Parsec

printStatement :: Parser Statement
printStatement = p pStrExpression
  where
    p expr = Print <$> (reserved "print" >> parens expr)

varDeclStatement ::
     String
  -> Parser e
  -> (Identifier -> e -> Statement)
  -> PType
  -> Parser Statement
varDeclStatement res exprP decl typ = do
  reserved res
  name <- identifier
  defined <- isDefinedInCurrentScopeM name
  when defined . fail $ "Already defined: " ++ show name
  reservedOp "="
  expr <- exprP
  insertVariableM name typ
  return $ decl name expr

blnVarDeclStatement :: Parser Statement
blnVarDeclStatement = varDeclStatement "bln" pBlnExpression BlnVarDecl PBln

intVarDeclStatement :: Parser Statement
intVarDeclStatement = varDeclStatement "int" pIntExpression IntVarDecl PInt

strVarDeclStatement :: Parser Statement
strVarDeclStatement = varDeclStatement "str" pStrExpression StrVarDecl PStr

blockStatement :: Parser Statement
blockStatement =
  fmap Block (enterBlockM *> braces (many1 (statement <* semi)) <* exitBlockM)

noopStatement :: Parser Statement
noopStatement = whiteSpace >> return Noop

statement :: Parser Statement
statement =
  printStatement <|> blnVarDeclStatement <|> intVarDeclStatement <|>
  strVarDeclStatement <|>
  noopStatement

statements :: Parser Statement
statements = do
  whiteSpace
    -- TODO: We allow input that doens't finish with a final ;
  list <- many1 (blockStatement <|> statement <* semi)
  eof
  return $ Seq list
