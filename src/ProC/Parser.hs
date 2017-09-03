{-# LANGUAGE DataKinds #-}
module ProC.Parser where

import ProC.Language

import Text.ParserCombinators.Parsec

symbol :: String -> Parser ()
symbol s = spaces >> string s >> spaces

stringLiteral = do
    s <- between (char '"') (char '"') . many $ noneOf "\""
    return $ StringLiteral s

printStatement :: Parser Statement
printStatement = do
  symbol "print"
  symbol "("
  s <- stringLiteral
  symbol ")"
  return $ Print s
  
noopStatement :: Parser Statement
noopStatement = spaces >> return Noop

statement :: Parser Statement
statement = do
    -- TODO: We allow input that doens't finish with a final ;
    list <- sepBy1 statement' (symbol ";")
    eof
    return $ Seq list
    where
        statement' = printStatement <|> noopStatement

parseProC :: String -> Either ParseError Statement
parseProC = parse statement "(Unknown)"
