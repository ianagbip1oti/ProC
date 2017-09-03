module ProC.Parser.StringExpression where

import ProC.Language
import ProC.Parser.Lexer

import Data.Functor.Identity

import Text.Parsec.Expr
import Text.Parsec.String

term :: Parser StringExpression
term = StringLiteral <$> stringLiteral

ops :: OperatorTable String () Identity StringExpression
ops = [ [ Infix (reservedOp "++" >> return StringConcat) AssocLeft ] ]

stringExpression :: Parser StringExpression
stringExpression = buildExpressionParser ops term