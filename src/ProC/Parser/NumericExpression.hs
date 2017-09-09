module ProC.Parser.NumericExpression where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.ProC

import           Control.Applicative

import           Text.Parsec.Expr

term :: Parser NumericExpression
term =
  parens numericExpression <|> IntLiteral <$> integer <|>
  IntVariable <$> identifier

ops :: POperatorTable NumericExpression
ops =
  [ [Prefix (op "-" (UnaryOp Negate))]
  , [inf "*" Multiply, inf "/" Divide]
  , [inf "+" Add, inf "-" Subtract]
  ]
  where
    inf s o = Infix (op s (BinOp o)) AssocLeft
    op s o = reservedOp s >> return o

numericExpression :: Parser NumericExpression
numericExpression = buildExpressionParser ops term
