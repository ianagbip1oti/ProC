{-# LANGUAGE DataKinds #-}
module ProC.Parser where

import ProC.Language
import ProC.Parser.Statement

import Text.Parsec

parseProC :: String -> Either ParseError ProCProgram
parseProC = parse statement "(Unknown)"
