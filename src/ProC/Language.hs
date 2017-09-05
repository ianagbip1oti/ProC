{-# LANGUAGE GADTs #-}
module ProC.Language where

type ProCProgram = Statement

data NumericExpression =
    IntLiteral Integer
    | IntVariable String
    -- Defining UnaryOp and BinOp with functions here may be
    -- limiting our ability to compile in the future
    | UnaryOp (Integer -> Integer) NumericExpression
    | BinOp (Integer -> Integer -> Integer) NumericExpression NumericExpression

data StringExpression where
  ToS :: NumericExpression -> StringExpression
  StringLiteral :: String -> StringExpression
  StringConcat :: StringExpression -> StringExpression -> StringExpression

data Statement where
    Noop :: Statement
    Print :: StringExpression -> Statement
    Seq :: [Statement] -> Statement
    IntVarDecl :: String -> NumericExpression -> Statement
