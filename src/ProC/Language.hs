{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module ProC.Language
  ( BlnExpression(..)
  , BlnUnaryOp(..)
  , BlnBinOp(..)
  , Identifier(..)
  , NumericBinOp(..)
  , NumericExpression(..)
  , NumericUnaryOp(..)
  , ProCProgram
  , Statement(..)
  , StringExpression(..)
  , PVar(..)
  , PType(..)
  , StrBinOp(..)
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

data BlnUnaryOp =
  Not
  deriving (Eq, Show)

data BlnBinOp
  = And
  | Or
  deriving (Eq, Show)

data BlnExpression
  = BlnLiteral Bool
  | BlnVariable (PVar 'PBln)
  | BlnUnaryOp BlnUnaryOp
               BlnExpression
  | BlnBinOp BlnBinOp
             BlnExpression
             BlnExpression
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
  | NumUnaryOp NumericUnaryOp
               NumericExpression
  | NumBinOp NumericBinOp
             NumericExpression
             NumericExpression
  deriving (Eq, Show)

data StrBinOp =
  Concat
  deriving (Eq, Show)

data StringExpression
  = ToS NumericExpression
  | StrLiteral String
  | StrVariable (PVar 'PStr)
  | StrBinOp StrBinOp
             StringExpression
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
