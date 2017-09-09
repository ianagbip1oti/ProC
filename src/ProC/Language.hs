module ProC.Language
  ( Identifier(..)
  , NumericBinOp(..)
  , NumericExpression(..)
  , NumericUnaryOp(..)
  , ProCProgram
  , Statement(..)
  , StringExpression(..)
  ) where

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

data StringExpression
  = ToS NumericExpression
  | StrLiteral String
  | StringConcat StringExpression
                 StringExpression
  deriving (Eq, Show)

data Statement
  = Noop
  | Print StringExpression
  | Seq [Statement]
  | IntVarDecl Identifier
               NumericExpression
  | StrVarDecl Identifier
               StringExpression
  deriving (Eq, Show)
