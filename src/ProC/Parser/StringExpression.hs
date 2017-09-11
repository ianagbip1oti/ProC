module ProC.Parser.StringExpression
  ( stringExpression
  ) where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.NumericExpression (numericExpression)
import           ProC.Parser.ProC

import           Control.Applicative
import           Control.Monad

import           Text.Parsec.Expr

term :: Parser StringExpression
term = parens stringExpression <|> lit <|> num <|> var
  where
    lit = StrLiteral <$> stringLiteral
    num = reserved "tos" >> ToS <$> parens numericExpression
    var = do
      ident <- identifier
      isValid <- isOfTypeM PStr ident
      unless isValid $ fail ("Not str variable: " ++ show ident)
      return . StrVariable $ PVar ident

ops :: POperatorTable StringExpression
ops = [[Infix (reservedOp "++" >> return StringConcat) AssocLeft]]

stringExpression :: Parser StringExpression
stringExpression = buildExpressionParser ops term
