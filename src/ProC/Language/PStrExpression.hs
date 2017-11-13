module ProC.Language.PStrExpression(PStrExpression(..), PStrBinOpr(..)) where

import ProC.Language.PIntExpression
import ProC.Language.PType

data PStrBinOpr =
  Concat
  deriving (Eq, Show)

data PStrExpression
  = ToS PIntExpression
  | PStrLiteral String
  | PStrVariable Identifier
  | PStrBinOpr PStrBinOpr
             PStrExpression
             PStrExpression
  deriving (Eq, Show)


