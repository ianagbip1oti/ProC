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

data NumericExpression =
    IntLiteral Integer
    | UnaryOp (Integer -> Integer) NumericExpression
    | BinOp (Integer -> Integer -> Integer) NumericExpression NumericExpression
    
instance Eval NumericExpression Integer where
    eval (IntLiteral i) = i
    eval (UnaryOp op e) = op $ eval e
    eval (BinOp op l r) = eval l `op` eval r

data StringExpression where
  ToS :: NumericExpression -> StringExpression
  StringLiteral :: String -> StringExpression
  StringConcat :: StringExpression -> StringExpression -> StringExpression

instance Eval StringExpression String where
    eval (StringLiteral s) = s
    eval (StringConcat l r) = mconcat $ toString . eval <$> [l, r]
    eval (ToS n) = toString $ eval n

data Statement where
    Noop :: Statement
    Print :: StringExpression -> Statement
    Seq :: [Statement] -> Statement
        
exec :: Statement -> IO ()
exec (Noop) = return ()
exec (Print s) = putStrLn $ eval s
exec (Seq ss) = forM_ ss exec