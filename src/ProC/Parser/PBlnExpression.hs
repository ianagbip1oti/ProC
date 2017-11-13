module ProC.Parser.PBlnExpression
  ( pBlnExpression
  ) where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.PIntExpression
import           ProC.Parser.ProC

import           Control.Applicative
import           Control.Monad

import           Text.Parsec.Expr
import           Text.Parsec.Prim           (try)

term :: Parser PBlnExpression
term = parens pBlnExpression <|> t <|> f <|> var <|> try pIntCmpExpression
  where
    t = reserved "tru" >> return (PBlnLiteral True)
    f = reserved "fls" >> return (PBlnLiteral False)
    var = do
      ident <- identifier
      isValid <- isOfTypeM PBln ident
      unless isValid $ fail ("Not bln variable: " ++ show ident)
      return $ PBlnVariable ident

ops :: POperatorTable PBlnExpression
ops = [[Prefix (op "!" (PBlnUnrOpr Not))], [inf "&&" And], [inf "||" Or]]
  where
    inf s o = Infix (op s (PBlnBinOpr o)) AssocLeft
    op s o = reservedOp s >> return o

pIntCmpExpression :: Parser PBlnExpression
pIntCmpExpression = do
  lhs <- pIntExpression
  op <-
    tryOp "==" PIntEq <|> tryOp "!=" PIntNotEq <|> tryOp "<=" PIntLTE <|>
    tryOp "<" PIntLT <|>
    tryOp ">=" PIntGTE <|>
    tryOp ">" PIntGT
  rhs <- pIntExpression
  return $ PIntCmpOpr op lhs rhs
  where
    tryOp o r = reservedOp o >> return r

pBlnExpression :: Parser PBlnExpression
pBlnExpression = buildExpressionParser ops term
