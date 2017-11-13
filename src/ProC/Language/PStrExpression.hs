module ProC.Language.PStrExpression(PStrExpression(..), PStrBinOpr(..)) where

import ProC.Language.PType

data PStrBinOpr =
  Concat
  deriving (Eq, Show)

data PStrExpression
  = ToS (Expression 'PInt)
  | PStrLiteral String
  | PStrVariable (PVar 'PStr)
  | PStrBinOp PStrBinOp
             StringExpression
             StringExpression
  deriving (Eq, Show)


