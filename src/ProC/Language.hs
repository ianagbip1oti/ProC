{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ProC.Language where

import Control.Monad

class ToString s where
    toString :: s -> String
    
instance ToString String where
    toString = id
    
instance ToString Integer where
    toString = show

class Eval exp res | exp -> res where
  eval :: exp -> res

data NumericExpression = IntLiteral Integer

instance Eval NumericExpression Integer where
    eval (IntLiteral i) = i

data StringExpression where
  StringFromNumericExpression :: NumericExpression -> StringExpression
  StringLiteral :: String -> StringExpression
  StringConcat :: StringExpression -> StringExpression -> StringExpression

instance Eval StringExpression String where
    eval (StringLiteral s) = s
    eval (StringConcat l r) = mconcat $ toString . eval <$> [l, r]
    eval (StringFromNumericExpression n) = toString $ eval n

data Statement where
    Noop :: Statement
    Print :: StringExpression -> Statement
    Seq :: [Statement] -> Statement
        
exec :: Statement -> IO ()
exec (Noop) = return ()
exec (Print s) = putStrLn $ eval s
exec (Seq ss) = forM_ ss exec