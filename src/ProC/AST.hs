{-# LANGUAGE DeriveFunctor #-}

module ProC.AST where

import Data.Text

import Data.Generics.Fixplate

data PType = PVoi | PStr

newtype Identifier = Id Text

data ExpressionF a
  = PStrLit Text
  deriving Functor
  
type Expression = Mu ExpressionF


data StatementF a
  = Noop
  | VarDcl PType Identifier (ExpressionF a)
  | Seq [a]
  deriving Functor
  
type Statement = Mu StatementF

