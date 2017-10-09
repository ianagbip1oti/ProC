module ProC.Parser.BlnExpression
  ( blnExpression
  ) where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.ProC

import           Control.Applicative
import           Control.Monad

import           Text.Parsec.Expr

term :: Parser BlnExpression
term = parens blnExpression <|> t <|> f <|> var
  where
    t = reserved "tru" >> return (BlnLiteral True)
    f = reserved "fls" >> return (BlnLiteral False)
    var = do
      ident <- identifier
      isValid <- isOfTypeM PBln ident
      unless isValid $ fail ("Not bln variable: " ++ show ident)
      return . BlnVariable $ PVar ident

blnExpression :: Parser BlnExpression
blnExpression = buildExpressionParser [] term
