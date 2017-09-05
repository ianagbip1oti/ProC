module ProC.Parser.StringExpression where

import ProC.Language
import ProC.Parser.Lexer
import ProC.Parser.NumericExpression (numericExpression)

import Control.Applicative

import Data.Functor.Identity

import Text.Parsec.Expr
import Text.Parsec.String

term :: Parser StringExpression
term = parens stringExpression <|> lit <|> num 
  where
    lit = StringLiteral <$> stringLiteral
    num = reserved "tos" >> ToS <$> parens numericExpression

ops :: OperatorTable String () Identity StringExpression
ops = [ [ Infix (reservedOp "++" >> return StringConcat) AssocLeft ] ]

stringExpression :: Parser StringExpression
stringExpression = buildExpressionParser ops term