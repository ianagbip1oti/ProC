{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module ProC.Language
  ( module ProC.Language.PType, BlnUnaryOp(..)
  , BlnBinOp(..)
  , Expression(..)
  , Identifier(..)
  , NumericBinOp(..)
  , NumericUnaryOp(..)
  , ProCProgram
  , Statement(..)
  , StringExpression(..)
  , StrBinOp(..)
  ) where

import ProC.Language.PType

type ProCProgram = Statement

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
  Variable :: Identifier -> Expression a
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
  | StrVariable Identifier
  | StrBinOp StrBinOp
             StringExpression
             StringExpression
  deriving (Eq, Show)

data Statement
  = Noop
  | Print StringExpression
  | Block [Statement]
  | Seq [Statement]
  | BlnVarDecl Identifier
               (Expression 'PBln)
  | IntVarDecl Identifier
               (Expression 'PInt)
  | StrVarDecl Identifier
               StringExpression
  deriving (Eq, Show)
