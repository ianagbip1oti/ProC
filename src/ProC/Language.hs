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

type ProCEnvM = StateT () IO

evalProCEnvM :: ProCEnvM a -> s -> IO a
evalProCEnvM f _ = evalStateT f ()

class ToString s where
    toString :: s -> String
    
instance ToString String where
    toString = id
    
instance ToString Integer where
    toString = show

class Eval exp res | exp -> res where
  eval :: exp -> ProCEnvM res

data NumericExpression =
    IntLiteral Integer
    | IntVariable String
    | UnaryOp (Integer -> Integer) NumericExpression
    | BinOp (Integer -> Integer -> Integer) NumericExpression NumericExpression
    
instance Eval NumericExpression Integer where
    eval (IntLiteral i) = return i
    eval (IntVariable _) = return 0 -- for now
    eval (UnaryOp op e) = eval e >>= return . op
    eval (BinOp op l r) = op <$> eval l <*> eval r

data StringExpression where
  ToS :: NumericExpression -> StringExpression
  StringLiteral :: String -> StringExpression
  StringConcat :: StringExpression -> StringExpression -> StringExpression

instance Eval StringExpression String where
    eval (StringLiteral s) = return s
    eval (StringConcat l r) = (++) <$> eval l <*> eval r
    eval (ToS n) = eval n >>= return . toString

data Statement where
    Noop :: Statement
    Print :: StringExpression -> Statement
    Seq :: [Statement] -> Statement
    IntVarDecl :: String -> NumericExpression -> Statement
        
exec :: Statement -> ProCEnvM ()
exec (IntVarDecl _ _) = return () -- for now
exec (Noop) = return ()
exec (Print s) = eval s >>= liftIO . putStrLn
exec (Seq ss) = forM_ ss exec

run :: ProCProgram -> IO ()
run p = evalProCEnvM (exec p) ()