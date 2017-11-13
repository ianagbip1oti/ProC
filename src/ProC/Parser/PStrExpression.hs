module ProC.Parser.PStrExpression
  ( pStrExpression
  ) where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.PIntExpression (pIntExpression)
import           ProC.Parser.ProC

import           Control.Applicative
import           Control.Monad

import           Text.Parsec.Expr

term :: Parser PStrExpression
term = parens pStrExpression <|> lit <|> num <|> var
  where
    lit = PStrLiteral <$> stringLiteral
    num = reserved "tos" >> ToS <$> parens pIntExpression
    var = do
      ident <- identifier
      isValid <- isOfTypeM PStr ident
      unless isValid $ fail ("Not str variable: " ++ show ident)
      return $ PStrVariable ident

ops :: POperatorTable PStrExpression
ops = [[Infix (reservedOp "++" >> return (PStrBinOpr Concat)) AssocLeft]]

pStrExpression :: Parser PStrExpression
pStrExpression = buildExpressionParser ops term
