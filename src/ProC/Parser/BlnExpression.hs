{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE TypeInType #-}

module ProC.Parser.BlnExpression
  ( blnExpression
  ) where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.ProC

import           Control.Applicative
import           Control.Monad

import           Text.Parsec.Expr

term :: Parser (Expression 'PBln)
term = parens blnExpression <|> t <|> f <|> var
  where
    t = reserved "tru" >> return (Literal True)
    f = reserved "fls" >> return (Literal False)
    var = do
      ident <- identifier
      isValid <- isOfTypeM PBln ident
      unless isValid $ fail ("Not bln variable: " ++ show ident)
      return $ Variable ident

ops :: POperatorTable (Expression 'PBln)
ops = [[Prefix (op "!" (UnaryOp Not))], [inf "&&" And], [inf "||" Or]]
  where
    inf s o = Infix (op s (BinaryOp o)) AssocLeft
    op s o = reservedOp s >> return o

blnExpression :: Parser (Expression 'PBln)
blnExpression = buildExpressionParser ops term
