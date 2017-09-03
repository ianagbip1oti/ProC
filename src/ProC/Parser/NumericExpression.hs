module ProC.Parser.NumericExpression where

import ProC.Language
import ProC.Parser.Lexer

import Data.Functor.Identity

import Text.Parsec.Expr
import Text.Parsec.String

term :: Parser NumericExpression
term = IntLiteral <$> integer

ops :: OperatorTable String () Identity NumericExpression
ops = [ ] 
--ops = [ [ Infix (reservedOp "++" >> return StringConcat) AssocLeft ] ]

numericExpression :: Parser NumericExpression
numericExpression = buildExpressionParser ops term