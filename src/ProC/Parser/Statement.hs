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
import           Text.Parsec.Prim           (try)

printStatement :: Parser Statement
printStatement = p pStrExpression
  where
    p expr = Print <$> (reserved "print" >> parens expr)

varDcl ::
     PType
  -> String
  -> Parser e
  -> (Identifier -> e -> Statement)
  -> Parser Statement
varDcl typ kyw expP dcl = do
  reserved kyw
  name <- identifier
  defined <- isDefinedInCurrentScopeM name
  when defined . fail $ "Already defined: " ++ show name
  reservedOp "="
  expr <- expP
  insertVariableM name typ
  return $ dcl name expr

varAss ::
     PType -> Parser e -> (Identifier -> e -> Statement) -> Parser Statement
varAss typ expP ass = do
  name <- identifier
  defined <- isOfTypeM typ name
  unless defined . fail $
    "Variable not of type: " ++ show typ ++ ", " ++ show name
  reservedOp "="
  expr <- expP
  return $ ass name expr

varStatement :: PType -> Parser Statement
varStatement t = vd t <|> try (va t)
  where
    vd PBln = varDcl PBln "bln" pBlnExpression PBlnVarDcl
    vd PInt = varDcl PInt "int" pIntExpression PIntVarDcl
    vd PStr = varDcl PStr "str" pStrExpression PStrVarDcl
    va PBln = varAss PBln pBlnExpression PBlnVarAss
    va PInt = varAss PInt pIntExpression PIntVarAss
    va PStr = varAss PStr pStrExpression PStrVarAss

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
  printStatement <|> varStatement PBln <|> varStatement PInt <|>
  varStatement PStr <|>
  noopStatement

statements :: Parser Statement
statements = do
  whiteSpace
    -- TODO: We allow input that doens't finish with a final ;
  list <- many1 (blockStatement <|> whlStatement <|> statement <* semi)
  eof
  return $ Seq list
