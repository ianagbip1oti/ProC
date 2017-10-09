module ProC.Parser.NumericExpression
  ( numericExpression
  ) where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.ProC

import           Control.Applicative
import           Control.Monad

import           Text.Parsec.Expr

term :: Parser NumericExpression
term = parens numericExpression <|> IntLiteral <$> integer <|> var
  where
    var = do
      ident <- identifier
      isValid <- isOfTypeM PInt ident
      unless isValid $ fail ("Not int variable: " ++ show ident)
      return . IntVariable $ PVar ident

ops :: POperatorTable NumericExpression
ops =
  [ [Prefix (op "-" (NumUnaryOp Negate))]
  , [inf "*" Multiply, inf "/" Divide]
  , [inf "+" Add, inf "-" Subtract]
  ]
  where
    inf s o = Infix (op s (NumBinOp o)) AssocLeft
    op s o = reservedOp s >> return o

numericExpression :: Parser NumericExpression
numericExpression = buildExpressionParser ops term
