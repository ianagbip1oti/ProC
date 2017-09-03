module ProC.Parser.NumericExpression where

import ProC.Language
import ProC.Parser.Lexer

import Control.Applicative

import Data.Functor.Identity

import Text.Parsec.Expr
import Text.Parsec.String

term :: Parser NumericExpression
term = IntLiteral <$> integer <|> parens numericExpression

ops :: OperatorTable String () Identity NumericExpression
ops = [ [ Prefix (reservedOp "-" >> return (UnaryOp (\n -> -n))) ] 
      , [ Infix  (reservedOp "*" >> return (BinOp (*))) AssocLeft
        , Infix  (reservedOp "/" >> return (BinOp (div))) AssocLeft
        ]
      , [ Infix  (reservedOp "+" >> return (BinOp (+))) AssocLeft
        , Infix  (reservedOp "-" >> return (BinOp (-))) AssocLeft
        ]
      ] 

numericExpression :: Parser NumericExpression
numericExpression = buildExpressionParser ops term