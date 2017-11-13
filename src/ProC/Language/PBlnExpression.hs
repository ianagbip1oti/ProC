module ProC.Language.PBlnExpression
  ( PBlnExpression(..)
  , PBlnUnrOpr(..)
  , PBlnBinOpr(..)
  , PIntCmpOpr(..)
  ) where

import           ProC.Language.PIntExpression
import           ProC.Language.PType

data PBlnUnrOpr =
  Not
  deriving (Eq, Show)

data PBlnBinOpr
  = And
  | Or
  deriving (Eq, Show)

data PIntCmpOpr
  = PIntEq
  | PIntNotEq
  | PIntGT
  | PIntGTE
  | PIntLT
  | PIntLTE
  deriving (Eq, Show)

data PBlnExpression
  = PBlnLiteral Bool
  | PBlnVariable Identifier
  | PBlnUnrOpr PBlnUnrOpr
               PBlnExpression
  | PBlnBinOpr PBlnBinOpr
               PBlnExpression
               PBlnExpression
  | PIntCmpOpr PIntCmpOpr
               PIntExpression
               PIntExpression
  deriving (Eq, Show)
