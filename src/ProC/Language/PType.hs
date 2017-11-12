module ProC.Language.PType (PType(..)) where

data PType
  = PBln
  | PInt
  | PStr
  deriving (Eq, Ord)
