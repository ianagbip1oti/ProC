{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module ProC.Language
  ( BlnUnaryOp(..)
  , BlnBinOp(..)
  , Expression(..)
  , Identifier(..)
  , NumericBinOp(..)
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

class ExpressionType (a :: PType) where
  type LiteralType a :: *
  type UnaryOpType a :: *
  type BinaryOpType a :: *

instance ExpressionType 'PBln where
  type LiteralType 'PBln = Bool
  type UnaryOpType 'PBln = BlnUnaryOp
  type BinaryOpType 'PBln = BlnBinOp

instance ExpressionType 'PInt where
  type LiteralType 'PInt = Integer
  type UnaryOpType 'PInt = NumericUnaryOp
  type BinaryOpType 'PInt = NumericBinOp

data Expression (a :: PType) where
  Literal :: LiteralType a -> Expression a
  Variable :: PVar a -> Expression a
  UnaryOp :: UnaryOpType a -> Expression a -> Expression a
  BinaryOp :: BinaryOpType a -> Expression a -> Expression a -> Expression a

deriving instance
         (Eq (LiteralType a), Eq (UnaryOpType a), Eq (BinaryOpType a)) =>
         Eq (Expression a)

deriving instance
         (Show (LiteralType a), Show (UnaryOpType a),
          Show (BinaryOpType a)) =>
         Show (Expression a)

data BlnUnaryOp =
  Not
  deriving (Eq, Show)

data BlnBinOp
  = And
  | Or
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

data StrBinOp =
  Concat
  deriving (Eq, Show)

data StringExpression
  = ToS (Expression 'PInt)
  | StrLiteral String
  | StrVariable (PVar 'PStr)
  | StrBinOp StrBinOp
             StringExpression
             StringExpression
  deriving (Eq, Show)

data Statement
  = Noop
  | Print StringExpression
  | Block [Statement]
  | Seq [Statement]
  | BlnVarDecl (PVar 'PBln)
               (Expression 'PBln)
  | IntVarDecl (PVar 'PInt)
               (Expression 'PInt)
  | StrVarDecl (PVar 'PStr)
               StringExpression
  deriving (Eq, Show)
