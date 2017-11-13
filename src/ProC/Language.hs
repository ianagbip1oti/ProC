{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module ProC.Language
  ( module ProC.Language.PIntExpression
  , module ProC.Language.PStrExpression, module ProC.Language.PType, BlnUnaryOp(..)
  , BlnBinOp(..)
  , Expression(..)
  , ProCProgram
  , Statement(..)
  ) where

import ProC.Language.PIntExpression
import ProC.Language.PStrExpression
import ProC.Language.PType

type ProCProgram = Statement

class ExpressionType (a :: PType) where
  type LiteralType a :: *
  type UnaryOpType a :: *
  type BinaryOpType a :: *

instance ExpressionType 'PBln where
  type LiteralType 'PBln = Bool
  type UnaryOpType 'PBln = BlnUnaryOp
  type BinaryOpType 'PBln = BlnBinOp

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

data Statement
  = Noop
  | Print PStrExpression
  | Block [Statement]
  | Seq [Statement]
  | BlnVarDecl Identifier
               (Expression 'PBln)
  | IntVarDecl Identifier
               PIntExpression
  | StrVarDecl Identifier
               PStrExpression
  deriving (Eq, Show)
