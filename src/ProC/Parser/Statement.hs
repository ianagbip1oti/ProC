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

varDclStatement ::
     String
  -> Parser e
  -> (Identifier -> e -> Statement)
  -> PType
  -> Parser Statement
varDclStatement res exprP dcl typ = do
  reserved res
  name <- identifier
  defined <- isDefinedInCurrentScopeM name
  when defined . fail $ "Already defined: " ++ show name
  reservedOp "="
  expr <- exprP
  insertVariableM name typ
  return $ dcl name expr

blnVarDclStatement :: Parser Statement
blnVarDclStatement = varDclStatement "bln" pBlnExpression BlnVarDcl PBln

intVarDclStatement :: Parser Statement
intVarDclStatement = varDclStatement "int" pIntExpression IntVarDcl PInt

strVarDclStatement :: Parser Statement
strVarDclStatement = varDclStatement "str" pStrExpression StrVarDcl PStr

blockStatement :: Parser Statement
blockStatement = Block <$> block

block :: Parser [Statement]
block = inBlockM . braces . many1 $ statement <* semi

whlStatement :: Parser Statement
whlStatement = do
  reserved "whl"
  cond <- parens pBlnExpression
  ss <- block
  return $ Whl cond ss

noopStatement :: Parser Statement
noopStatement = whiteSpace >> return Noop

statement :: Parser Statement
statement =
  printStatement <|> blnVarDclStatement <|> intVarDclStatement <|>
  strVarDclStatement <|>
  noopStatement

statements :: Parser Statement
statements = do
  whiteSpace
    -- TODO: We allow input that doens't finish with a final ;
  list <- many1 (blockStatement <|> whlStatement <|> statement <* semi)
  eof
  return $ Seq list
