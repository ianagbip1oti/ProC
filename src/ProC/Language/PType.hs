module ProC.Language.PType (Identifier(..), PType(..)) where

data PType
  = PBln
  | PInt
  | PStr
  deriving (Eq, Ord)
  

newtype Identifier =
  Identifier String
  deriving (Eq, Ord, Show)


