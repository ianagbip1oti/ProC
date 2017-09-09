{-# LANGUAGE GADTs #-}

module ProC.Language where

type ProCProgram = Statement

newtype Identifier =
  Identifier String
  deriving (Eq, Ord, Show)

data NumericUnaryOp =
  Negate
  deriving (Eq, Show)

data NumericBinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Eq, Show)

data NumericExpression
  = IntLiteral Integer
  | IntVariable Identifier
  | UnaryOp NumericUnaryOp
            NumericExpression
  | BinOp NumericBinOp
          NumericExpression
          NumericExpression
  deriving (Eq, Show)

data StringExpression where
  ToS :: NumericExpression -> StringExpression
  StringLiteral :: String -> StringExpression
  StringConcat :: StringExpression -> StringExpression -> StringExpression
  deriving (Eq, Show)

data Statement where
  Noop :: Statement
  Print :: StringExpression -> Statement
  Seq :: [Statement] -> Statement
  IntVarDecl :: Identifier -> NumericExpression -> Statement
