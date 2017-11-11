{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE TypeInType #-}

module ProC.Parser.BlnExpression
  ( blnExpression
  , checkExpression
  ) where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.NumericExpression
import           ProC.Parser.ProC

import           Control.Applicative
import           Control.Monad

import           Text.Parsec.Expr
import           Text.Parsec.Prim              (try)

term :: Parser (Expression 'PBln)
term = parens blnExpression <|> t <|> f <|> var
  where
    t = reserved "tru" >> return (Literal True)
    f = reserved "fls" >> return (Literal False)
    var = do
      ident <- identifier
      isValid <- isOfTypeM PBln ident
      unless isValid $ fail ("Not bln variable: " ++ show ident)
      return . Variable $ PVar ident

ops :: POperatorTable (Expression 'PBln)
ops = [[Prefix (op "!" (UnaryOp Not))], [inf "&&" And], [inf "||" Or]]
  where
    inf s o = Infix (op s (BinaryOp o)) AssocLeft
    op s o = reservedOp s >> return o

blnExpression :: Parser (Expression 'PBln)
blnExpression = buildExpressionParser ops term

comparisonExpression :: Parser ComparisonExpression
comparisonExpression = do
  lhs <- numericExpression
  op <-
    tryOp "==" NumericEq <|> tryOp "!=" NumericNotEq <|> tryOp "<=" NumericLTE <|>
    tryOp "<" NumericLT <|>
    tryOp ">=" NumericGTE <|>
    tryOp ">" NumericGT
  rhs <- numericExpression
  return $ PIntCompare op lhs rhs
  where
    tryOp o r = reservedOp o >> return r

checkExpression :: Parser Check
checkExpression = try blnExpr <|> compExpr
  where
    blnExpr = CheckE <$> blnExpression
    compExpr = CheckC <$> comparisonExpression
