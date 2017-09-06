{-# LANGUAGE DataKinds #-}
module ProC.Parser where

import ProC.Language
import ProC.Parser.ProC
import ProC.Parser.Statement

import Text.Parsec (ParseError)

parseProC :: String -> Either ParseError ProCProgram
parseProC = parse statement
