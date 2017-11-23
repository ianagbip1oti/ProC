{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ProC.Parser where

import           ProC.AST

import           Data.Text
import           Data.Void

import           Data.Generics.Fixplate

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 lineCmt blockCmt
  where
    lineCmt = L.skipLineComment "//"
    blockCmt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

semi :: Parser Text
semi = symbol ";"

stringLiteral :: Parser Text
stringLiteral = pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

reservedWords :: [Text]
reservedWords = ["int"]

reserved :: Text -> Parser ()
reserved w =
  if w `elem` reservedWords
    then lexeme $ string w *> notFollowedBy alphaNumChar
    else fail $ show w ++ " is not a reserved word"

identifier :: Parser Identifier
identifier = (lexeme . try) (p >>= check)
  where
    p = cons <$> letterChar <*> (pack <$> many alphaNumChar)
    check x =
      if x `elem` reservedWords
        then fail $ "reserved word " ++ show x ++ " cannot be an identifier"
        else return $ Id x

expr :: Parser s -> Parser (ExpressionF s)
expr _ = PStrLit <$> stringLiteral

seqStmt :: Parser s -> Parser (StatementF s)
seqStmt s = Seq <$> sepBy1 s semi

stmt :: Parser s -> Parser (StatementF s)
stmt a = varDclStmt
  where
    varDclStmt = do
      reserved "str"
      i <- identifier
      _ <- symbol "="
      e <- expr a
      return $ VarDcl PStr i e

program :: Parser a -> Parser (StatementF a)
program s = between sc eof (seqStmt s)

fixParser :: Functor f => (forall a. Parser a -> Parser (f a)) -> Parser (Mu f)
fixParser f = Fix <$> f (fixParser f)

parse :: Parser Statement
parse = fixParser program
