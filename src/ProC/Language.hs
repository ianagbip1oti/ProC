{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ProC.Language where

import Control.Monad
import Control.Monad.State

type ProCProgram = Statement

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
    | IntVariable String
    | UnaryOp (Integer -> Integer) NumericExpression
    | BinOp (Integer -> Integer -> Integer) NumericExpression NumericExpression
    
instance Eval NumericExpression Integer where
    eval (IntLiteral i) = i
    eval (IntVariable _) = 0 -- for now
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
    IntVarDecl :: String -> NumericExpression -> Statement
        
exec :: Statement -> StateT () IO ()
exec (IntVarDecl _ _) = return () -- for now
exec (Noop) = return ()
exec (Print s) = liftIO . putStrLn $ eval s
exec (Seq ss) = forM_ ss exec

run :: ProCProgram -> IO ()
run p = evalStateT (exec p) ()