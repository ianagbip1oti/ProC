{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module ProC.Interpreter
  ( runProC
  ) where

import           ProC.Interpreter.Context
import           ProC.Language

import           Control.Monad.State

class ToString s where
  toString :: s -> String

instance ToString String where
  toString = id

instance ToString Integer where
  toString = show

class Eval exp res | exp -> res where
  eval :: exp -> ContextM res

instance Eval (Expression 'PBln) Bool where
  eval (Literal b)        = return b
  eval (Variable n)       = getVarM n
  eval (UnaryOp Not b)    = not <$> eval b
  eval (BinaryOp And l r) = (&&) <$> eval l <*> eval r
  eval (BinaryOp Or l r)  = (||) <$> eval l <*> eval r

instance Eval (Expression 'PInt) Integer where
  eval (Literal i) = return i
  eval (Variable n) = getVarM n
  eval (UnaryOp Negate e) = negate <$> eval e
  eval (BinaryOp o l r) =
    case o of
      Add      -> op (+) l r
      Subtract -> op (-) l r
      Multiply -> op (*) l r
      Divide   -> op div l r
    where
      op f x y = f <$> eval x <*> eval y

instance Eval StringExpression String where
  eval (StrLiteral s)        = return s
  eval (StrVariable s)       = getVarM s
  eval (StrBinOp Concat l r) = (++) <$> eval l <*> eval r
  eval (ToS n)               = toString <$> eval n

exec :: Statement -> ContextM ()
exec (BlnVarDecl n e) = eval e >>= setVarM n
exec (IntVarDecl n e) = eval e >>= setVarM n
exec (StrVarDecl n e) = eval e >>= setVarM n
exec Noop             = return ()
exec (Print s)        = eval s >>= liftIO . putStrLn
exec (Seq ss)         = forM_ ss exec

runProC :: ProCProgram -> IO ()
runProC p = evalContextM (exec p)
