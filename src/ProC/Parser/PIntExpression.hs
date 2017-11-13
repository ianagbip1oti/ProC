{-# LANGUAGE DataKinds #-}

module ProC.Parser.PIntExpression
  ( pIntExpression
  ) where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.ProC

import           Control.Applicative
import           Control.Monad

import           Text.Parsec.Expr

term :: Parser PIntExpression
term = parens pIntExpression <|> PIntLiteral <$> integer <|> var
  where
    var = do
      ident <- identifier
      isValid <- isOfTypeM PInt ident
      unless isValid $ fail ("Not int variable: " ++ show ident)
      return $ PIntVariable ident

ops :: POperatorTable PIntExpression
ops =
  [ [Prefix (op "-" (PIntUnrOpr Negate))]
  , [inf "*" Multiply, inf "/" Divide]
  , [inf "+" Add, inf "-" Subtract]
  ]
  where
    inf s o = Infix (op s (PIntBinOpr o)) AssocLeft
    op s o = reservedOp s >> return o

pIntExpression :: Parser PIntExpression
pIntExpression = buildExpressionParser ops term
