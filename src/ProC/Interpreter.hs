{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ProC.Interpreter where

import ProC.Interpreter.Context
import ProC.Language

import Control.Monad.State

class ToString s where
    toString :: s -> String
    
instance ToString String where
    toString = id
    
instance ToString Integer where
    toString = show


class Eval exp res | exp -> res where
  eval :: exp -> ContextM res
    
instance Eval NumericExpression Integer where
    eval (IntLiteral i) = return i
    eval (IntVariable n) = getVarM n
    eval (UnaryOp op e) = eval e >>= return . op
    eval (BinOp op l r) = op <$> eval l <*> eval r

instance Eval StringExpression String where
    eval (StringLiteral s) = return s
    eval (StringConcat l r) = (++) <$> eval l <*> eval r
    eval (ToS n) = eval n >>= return . toString

    
exec :: Statement -> ContextM ()
exec (IntVarDecl n e) = eval e >>= setVarM n
exec (Noop) = return ()
exec (Print s) = eval s >>= liftIO . putStrLn
exec (Seq ss) = forM_ ss exec

run :: ProCProgram -> IO ()
run p = evalContextM (exec p)
