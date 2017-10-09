{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module ProC.Language
  ( BlnExpression(..)
  , Identifier(..)
  , NumericBinOp(..)
  , NumericExpression(..)
  , NumericUnaryOp(..)
  , ProCProgram
  , Statement(..)
  , StringExpression(..)
  , PVar(..)
  , PType(..)
  , getIdentifier
  ) where

type ProCProgram = Statement

data PType
  = PBln
  | PInt
  | PStr
  deriving (Eq, Ord)

data PVar :: PType -> * where
  PVar :: Identifier -> PVar a
  deriving (Eq, Ord, Show)

getIdentifier :: PVar a -> Identifier
getIdentifier (PVar i) = i

newtype Identifier =
  Identifier String
  deriving (Eq, Ord, Show)

data BlnExpression
  = BlnLiteral Bool
  | BlnVariable (PVar 'PBln)
  deriving (Eq, Show)

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
  | StrVariable (PVar 'PStr)
  | StringConcat StringExpression
                 StringExpression
  deriving (Eq, Show)

data Statement
  = Noop
  | Print StringExpression
  | Seq [Statement]
  | BlnVarDecl (PVar 'PBln)
               BlnExpression
  | IntVarDecl (PVar 'PInt)
               NumericExpression
  | StrVarDecl (PVar 'PStr)
               StringExpression
  deriving (Eq, Show)
