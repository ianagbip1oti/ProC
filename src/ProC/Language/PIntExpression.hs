module ProC.Language.PIntExpression
  ( PIntExpression(..)
  , PIntUnrOpr(..)
  , PIntBinOpr(..)
  ) where

import           ProC.Language.PType

data PIntUnrOpr =
  Negate
  deriving (Eq, Show)

data PIntBinOpr
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Eq, Show)

data PIntExpression
  = PIntLiteral Integer
  | PIntVariable Identifier
  | PIntUnrOpr PIntUnrOpr
               PIntExpression
  | PIntBinOpr PIntBinOpr
               PIntExpression
               PIntExpression
  deriving (Eq, Show)
