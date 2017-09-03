{-# LANGUAGE DataKinds #-}
module ProC.Parser where

import ProC.Language

import Debug.Trace

import Text.Parsec.Language
import Text.ParserCombinators.Parsec

symbol s = spaces >> string s >> spaces

stringLiteral = between (char '"') (char '"') . many $ noneOf "\""

printStatement :: Parser Statement
printStatement = do
  symbol "print"
  symbol "("
  s <- stringLiteral
  symbol ")"
  symbol ";"
  return $ Print s

statement :: Parser Statement
statement = printStatement

parseProC :: String -> Either ParseError Statement
parseProC = parse statement "(Unknown)"
