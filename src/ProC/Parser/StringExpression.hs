module ProC.Parser.StringExpression
  ( stringExpression
  ) where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.NumericExpression (numericExpression)
import           ProC.Parser.ProC

import           Control.Applicative

import           Text.Parsec.Expr

term :: Parser StringExpression
term = parens stringExpression <|> lit <|> num
  where
    lit = StringLiteral <$> stringLiteral
    num = reserved "tos" >> ToS <$> parens numericExpression

ops :: POperatorTable StringExpression
ops = [[Infix (reservedOp "++" >> return StringConcat) AssocLeft]]

stringExpression :: Parser StringExpression
stringExpression = buildExpressionParser ops term
