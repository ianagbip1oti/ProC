module ProC.Parser.NumericExpression where

import ProC.Language
import ProC.Parser.Lexer
import ProC.Parser.ProC

import Control.Applicative

import Text.Parsec.Expr

term :: Parser NumericExpression
term = parens numericExpression
   <|> IntLiteral <$> integer
   -- TODO: Check the variable has been defined, and the type
   <|> IntVariable <$> identifier

ops :: POperatorTable NumericExpression
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