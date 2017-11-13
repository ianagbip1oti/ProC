module ProC.Language.PBlnExpression(PBlnExpression(..), PBlnUnrOpr(..), PBlnBinOpr(..)) where

import ProC.Language.PType

data PBlnUnrOpr =
  Not
  deriving (Eq, Show)

data PBlnBinOpr
  = And
  | Or
  deriving (Eq, Show)

data PBlnExpression
  = PBlnLiteral Bool
  | PBlnVariable Identifier
  | PBlnUnrOpr PBlnUnrOpr PBlnExpression
  | PBlnBinOpr PBlnBinOpr PBlnExpression PBlnExpression
  deriving (Eq, Show)


