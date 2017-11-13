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

instance Eval PBlnExpression Bool where
  eval (PBlnLiteral b)      = return b
  eval (PBlnVariable n)     = getVarM n
  eval (PBlnUnrOpr Not b)   = not <$> eval b
  eval (PBlnBinOpr And l r) = (&&) <$> eval l <*> eval r
  eval (PBlnBinOpr Or l r)  = (||) <$> eval l <*> eval r

instance Eval PIntExpression Integer where
  eval (PIntLiteral i) = return i
  eval (PIntVariable n) = getVarM n
  eval (PIntUnrOpr Negate e) = negate <$> eval e
  eval (PIntBinOpr o l r) =
    case o of
      Add      -> op (+) l r
      Subtract -> op (-) l r
      Multiply -> op (*) l r
      Divide   -> op div l r
    where
      op f x y = f <$> eval x <*> eval y

instance Eval PStrExpression String where
  eval (PStrLiteral s)         = return s
  eval (PStrVariable s)        = getVarM s
  eval (PStrBinOpr Concat l r) = (++) <$> eval l <*> eval r
  eval (ToS n)                 = toString <$> eval n

exec :: Statement -> ContextM ()
exec (BlnVarDcl n e) = eval e >>= setVarM n
exec (IntVarDcl n e) = eval e >>= setVarM n
exec (StrVarDcl n e) = eval e >>= setVarM n
exec Noop            = return ()
exec (Print s)       = eval s >>= liftIO . putStrLn
exec (Seq ss)        = forM_ ss exec
exec (Block ss)      = enterBlockM >> forM_ ss exec >> exitBlockM

runProC :: ProCProgram -> IO ()
runProC p = evalContextM (exec p)
