module ProC.Parser.PBlnExpression
  ( pBlnExpression
  ) where

import           ProC.Language
import           ProC.Parser.Lexer
import           ProC.Parser.ProC

import           Control.Applicative
import           Control.Monad

import           Text.Parsec.Expr

term :: Parser PBlnExpression
term = parens pBlnExpression <|> t <|> f <|> var
  where
    t = reserved "tru" >> return (PBlnLiteral True)
    f = reserved "fls" >> return (PBlnLiteral False)
    var = do
      ident <- identifier
      isValid <- isOfTypeM PBln ident
      unless isValid $ fail ("Not bln variable: " ++ show ident)
      return $ PBlnVariable ident

ops :: POperatorTable PBlnExpression
ops = [[Prefix (op "!" (PBlnUnrOpr Not))], [inf "&&" And], [inf "||" Or]]
  where
    inf s o = Infix (op s (PBlnBinOpr o)) AssocLeft
    op s o = reservedOp s >> return o

pBlnExpression :: Parser PBlnExpression
pBlnExpression = buildExpressionParser ops term
