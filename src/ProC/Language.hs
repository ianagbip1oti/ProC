{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module ProC.Language
  ( Identifier(..)
  , NumericBinOp(..)
  , NumericExpression(..)
  , NumericUnaryOp(..)
  , ProCProgram
  , Statement(..)
  , StringExpression(..)
  , PVar(..)
  , PType(..)
  ) where

type ProCProgram = Statement

data PType = PInt

data PVar :: PType -> * where
  PVar :: Identifier -> PVar a
  deriving (Eq, Ord, Show)

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
  | IntVariable (PVar 'PInt)
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
  | IntVarDecl (PVar 'PInt)
               NumericExpression
  | StrVarDecl Identifier
               StringExpression
  deriving (Eq, Show)
