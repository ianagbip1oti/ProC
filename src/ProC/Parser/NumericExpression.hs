{-# LANGUAGE DataKinds #-}

module ProC.Parser.NumericExpression
  ( numericExpression
  ) where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.ProC

import           Control.Applicative
import           Control.Monad

import           Text.Parsec.Expr

term :: Parser (Expression 'PInt)
term = parens numericExpression <|> Literal <$> integer <|> var
  where
    var = do
      ident <- identifier
      isValid <- isOfTypeM PInt ident
      unless isValid $ fail ("Not int variable: " ++ show ident)
      return $ Variable ident

ops :: POperatorTable (Expression 'PInt)
ops =
  [ [Prefix (op "-" (UnaryOp Negate))]
  , [inf "*" Multiply, inf "/" Divide]
  , [inf "+" Add, inf "-" Subtract]
  ]
  where
    inf s o = Infix (op s (BinaryOp o)) AssocLeft
    op s o = reservedOp s >> return o

numericExpression :: Parser (Expression 'PInt)
numericExpression = buildExpressionParser ops term
